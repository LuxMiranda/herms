{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

-- silencing GHC warnings, and for compatibility with base < 4.9.0

import AddCLI
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Aeson as Json
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid ()
import Data.Ratio
import Data.String (IsString (..))
import qualified Data.Yaml as Yaml
import qualified Export.Format as EF
import Export.Html (htmlDoc, toHtml)
import Export.Org (toOrg)
import Find
import Foreign.C.Error
import GHC.IO.Exception
import qualified Import.Format as IF
import qualified Lang.Strings as Str
import Options.Applicative
import ReadConfig
import RichText
import System.Directory
import System.Exit (exitFailure)
import System.IO
import Text.Read
import Types
import UnitConversions
import Utils

-- Global constants
versionStr :: String
versionStr = "2.0.1"

type HermsReader = ReaderT (Config, RecipeBook)

-- | @getRecipeBookWith reads in recipe book with already read-in config
--
-- It first attempts to read the new YAML recipe format, and then the
-- deprecated one. If reading the YAML fails but the older format works, then
-- it warns the user about deprecation of the old format.
getRecipeBookWith :: Config -> IO (Either Yaml.ParseException [Recipe])
getRecipeBookWith config = do
  content <- readFileOrDefault "recipes.yaml" (recipesFile' config)
  case Yaml.decodeEither' content of
    Right recipes -> pure (Right recipes)
    Left err ->
      case traverse readEither $ lines (BS.unpack content) of
        Left _ -> pure (Left err)
        Right recipes ->
          let u = "https://github.com/JackKiefer/herms/tree/master/CHANGELOG.md"
           in putStrLn ("You are using a deprecated recipe format. See " ++ u)
                >> pure (Right recipes)

getRecipe :: String -> [Recipe] -> Maybe Recipe
getRecipe target = find ((target ==) . recipeName)

saveOrDiscard ::
  [[String]] -> -- input for the new recipe
  Maybe Recipe -> -- maybe an original recipe prior to any editing
  HermsReader IO ()
saveOrDiscard input oldRecp = do
  (config, recipeBook) <- ask
  let t = translator config
  let newRecipe = readRecipe input
  liftIO $ putTextLn $ showRecipe t newRecipe Nothing
  liftIO $ putStrLn (t Str.saveRecipeYesNoEdit)
  response <- liftIO getLine
  if response == t Str.y || response == t Str.yCap
    then do
      let recpName = maybe (recipeName newRecipe) recipeName oldRecp
      unless (isNothing (readRecipeRef recpName recipeBook)) $ removeSilent [recpName]
      liftIO $ BS.appendFile (recipesFile' config) (Yaml.encode [newRecipe] `BS.snoc` '\n')
      liftIO $ putStrLn (t Str.recipeSaved)
    else
      if response == t Str.n || response == t Str.nCap
        then liftIO $ putStrLn (t Str.changesDiscarded)
        else
          if response == t Str.e || response == t Str.eCap
            then doEdit newRecipe oldRecp
            else do
              liftIO $ putStrLn ("\n" ++ t Str.badEntry ++ "\n")
              saveOrDiscard input oldRecp

add :: HermsReader IO ()
add = do
  (config, _) <- ask
  input <- liftIO $ getAddInput (translator config)
  saveOrDiscard input Nothing

doEdit :: Recipe -> Maybe Recipe -> HermsReader IO ()
doEdit recp origRecp = do
  (config, _) <- ask
  input <- liftIO $ getEdit (translator config) (recipeName recp) (description recp) serving amounts units ingrs attrs dirs tag
  saveOrDiscard input origRecp
  where
    serving = show $ servingSize recp
    ingrList = adjustIngredients (servingSize recp % 1) $ ingredients recp
    toStr f = unlines (map f ingrList)
    amounts = toStr (showFrac . quantity)
    units = toStr (showUnit . unit)
    ingrs = toStr ingredientName
    dirs = unlines (directions recp)
    attrs = toStr attribute
    tag = unwords (tags recp)

edit :: String -> String -> HermsReader IO ()
edit target convName = do
  (config, recipeBook) <- ask
  let t = translator config
  let (_, conv) = getServingsAndConv 0 convName config
  case readRecipeRef target recipeBook of
    Nothing -> liftIO $ putStrLn $ target ++ t Str.doesNotExist
    Just recp -> doEdit (convertRecipeUnits conv recp) (Just recp)

-- Only supports editing one recipe per command

-- | `readRecipeRef target book` interprets the string `target`
--   as either an index or a recipe's name and looks up the
--   corresponding recipe in the `book`
readRecipeRef :: String -> [Recipe] -> Maybe Recipe
readRecipeRef target recipeBook =
  (safeLookup recipeBook . pred =<< readMaybe target)
    <|> getRecipe target recipeBook

importFile :: String -> String -> HermsReader IO ()
importFile target format = do
  (config, recipeBook) <- ask
  let t = translator config

  -- Read the new recipe book from the filesystem
  fmt <- liftIO $
    case IF.readImportFormat format of
      Just f -> return f
      Nothing ->
        putStrLn (t Str.unsupportedFormat ++ format)
          >> putStrLn (t Str.supportedFormats)
          >> exitFailure
  otherRecipeBook <- (liftIO :: IO [Recipe] -> HermsReader IO [Recipe]) $
    case fmt of
      IF.JSON ->
        ((Json.eitherDecodeStrict' <$> BS.readFile target) :: IO (Either String [Recipe]))
          >>= \case
            Right book -> pure (book :: [Recipe])
            Left err -> putStrLn err >> exitFailure
      IF.YAML ->
        (Yaml.decodeFileEither target :: IO (Either Yaml.ParseException [Recipe]))
          >>= \case
            Right book -> pure (book :: [Recipe])
            Left err -> print err >> exitFailure

  -- Combine the new recipes with the old
  let recipeEq = (==) `on` recipeName

  -- Overwrite the old recipe file
  replaceRecipeBook $
    deleteFirstsBy recipeEq recipeBook otherRecipeBook ++ otherRecipeBook

  liftIO $
    if null otherRecipeBook
      then putStrLn (t Str.nothingToImport)
      else do
        putStrLn (t Str.importedRecipes)
        forM_ otherRecipeBook $ \recipe ->
          putStrLn $ "  " ++ recipeName recipe

export :: [String] -> String -> HermsReader IO ()
export targets format = do
  (config, recipeBook) <- ask
  let t = translator config
  fmt <- liftIO $
    case EF.readExportFormat format of
      Just f -> return f
      Nothing ->
        putStrLn (t Str.unsupportedFormat ++ format)
          >> putStrLn (t Str.supportedFormats)
          >> exitFailure
  let realTargets =
        if null targets
          then Just recipeBook
          else traverse (flip readRecipeRef recipeBook) targets
  -- TODO: error message should say /which/ recipe doesn't exist
  liftIO . putText $
    case realTargets of
      Nothing -> t Str.doesNotExist
      Just recipes -> fromString $
        case fmt of
          EF.JSON -> BSL.unpack $ Json.encode recipes
          EF.YAML -> BS.unpack $ Yaml.encode recipes
          EF.HTML -> htmlDoc (Just "/style.css") $ concatMap toHtml recipes
          EF.Org -> toOrg recipes

getServingsAndConv :: Int -> String -> Config -> (Maybe Int, Conversion)
getServingsAndConv serv convName config = (servings, conv)
  where
    servings =
      if
          | serv <= 0 && defaultServingSize' config == 0 -> Nothing
          | serv <= 0 -> Just (defaultServingSize' config)
          | otherwise -> Just serv
    t = translator config
    conv
      | convName == t Str.metric = Metric
      | convName == t Str.imperial = Imperial
      | otherwise = defaultUnit' config

findRecipes :: String -> HermsReader IO ()
findRecipes rgx = do
  (_, recipeBook) <- ask
  let matches = findMatches rgx recipeBook
  liftIO $ mapM_ putStrLn matches

view :: [String] -> Int -> String -> HermsReader IO ()
view targets serv convName = do
  (config, recipeBook) <- ask
  let t = translator config
  let (servings, conv) = getServingsAndConv serv convName config
  liftIO $
    forM_ targets $ \target ->
      putText $ case readRecipeRef target recipeBook of
        Nothing -> target ~~ t Str.doesNotExist
        Just recp -> showRecipe t (convertRecipeUnits conv recp) servings

viewByStep :: [String] -> Int -> String -> HermsReader IO ()
viewByStep targets serv convName = do
  (config, recipeBook) <- ask
  let t = translator config
  let (servings, conv) = getServingsAndConv serv convName config
  liftIO $ hSetBuffering stdout NoBuffering
  forM_ targets $ \target -> case readRecipeRef target recipeBook of
    Nothing -> liftIO $ putStr $ target ++ t Str.doesNotExist
    Just recp -> viewRecipeByStep (convertRecipeUnits conv recp) servings

viewRecipeByStep :: Recipe -> Maybe Int -> HermsReader IO ()
viewRecipeByStep recp servings = do
  (config, _) <- ask
  let t = translator config
  liftIO $ putText $ showRecipeHeader t recp servings
  let steps = showRecipeSteps recp
  liftIO $
    forM_ (init steps) $ \step -> do
      putStr $ step ++ t Str.more
      getLine
  liftIO $ putStr $ last steps ++ "\n"

list :: [String] -> Bool -> Bool -> HermsReader IO ()
list inputTags groupByTags nameOnly = do
  (_, recipes) <- ask
  let recipesWithIndex = zip [1 ..] recipes
  let targetRecipes = filterByTags inputTags recipesWithIndex
  if groupByTags
    then listByTags nameOnly targetRecipes
    else listDefault nameOnly targetRecipes

filterByTags :: [String] -> [(Int, Recipe)] -> [(Int, Recipe)]
filterByTags [] = id
filterByTags inputTags = filter (inTags . tags . snd)
  where
    inTags r = all (`elem` r) inputTags

listDefault :: Bool -> [(Int, Recipe)] -> HermsReader IO ()
listDefault nameOnly (unzip -> (indices, recipes)) = do
  (config, _) <- ask
  let recipeList = map (showRecipeInfo (translator config)) recipes
      size = length $ show $ length recipeList
      strIndices = map (padLeft size . show) indices
  liftIO $
    if nameOnly
      then mapM_ (putStrLn . recipeName) recipes
      else mapM_ putTextLn $ zipWith (\i -> ((i ~~ ". ") ~~)) strIndices recipeList

listByTags :: Bool -> [(Int, Recipe)] -> HermsReader IO ()
listByTags nameOnly recipesWithIdx = do
  (config, _) <- ask
  let tagsRecipes :: [[(String, (Int, Recipe))]]
      tagsRecipes =
        groupBy ((==) `on` fst) $
          sortBy (compare `on` fst) $
            concat $
              flip map recipesWithIdx $ \recipeWithIdx ->
                map (,recipeWithIdx) $ tags $ snd recipeWithIdx
  liftIO $
    forM_ tagsRecipes $ \tagRecipes -> do
      putTextLn $ bold $ fontColor Magenta $ fst $ head tagRecipes -- Tag name
      forM_ (map snd tagRecipes) $ \(i, recipe) ->
        if nameOnly
          then putStrLn $ recipeName recipe
          else putTextLn $ "  " ~~ show i ~~ ". " ~~ showRecipeInfo (translator config) recipe
      putStrLn ""

showRecipeInfo :: Translator -> Recipe -> RichText
showRecipeInfo t recipe = name ~~ "\n\t" ~~ desc ~~ "\n\t[" ~~ tagsStr ~~ ": " ~~ showTags ~~ "]"
  where
    name = fontColor Blue $ recipeName recipe
    desc = (takeFullWords . description) recipe
    showTags = fontColor Green $ (intercalate ", " . tags) recipe
    tagsStr = fontColor White (t Str.capTags)

takeFullWords :: String -> String
takeFullWords = unwords . takeFullWords' 0 . words
  where
    takeFullWords' _ [] = []
    takeFullWords' n [x]
      | (length x + n) > 40 = []
      | otherwise = [x]
    takeFullWords' n (x : xs)
      | (length x + n) > 40 = [x ++ "..."]
      | otherwise =
        x : takeFullWords' (length x + n) xs

-- | @replaceDataFile fp str@ replaces the target data file @fp@ with
--   the new content @str@ in a safe manner: it opens a temporary file
--   first, writes to it, closes the handle, removes the target,
--   and finally moves the temporary file over to the target @fp@.
replaceDataFile :: FilePath -> BS.ByteString -> IO ()
replaceDataFile fileName str = do
  (tempName, tempHandle) <- openTempFile "." "herms_temp"
  BS.hPut tempHandle str
  hClose tempHandle
  removeFile fileName
  let exdev e =
        if ioe_errno e == Just ((\(Errno a) -> a) eXDEV)
          then copyFile tempName fileName >> removeFile tempName
          else throw e
  renameFile tempName fileName `catch` exdev

replaceRecipeBook :: RecipeBook -> HermsReader IO ()
replaceRecipeBook newBook = do
  (config, _) <- ask
  liftIO $ replaceDataFile (recipesFile' config) (Yaml.encode newBook)

-- | @removeWithVerbosity v recipes@ deletes the @recipes@ from the
--   book, listing its work only if @v@ is set to @True@.
--   This subsumes both @remove@ and @removeSilent@.
removeWithVerbosity :: Bool -> [String] -> HermsReader IO ()
removeWithVerbosity v targets = do
  (config, recipeBook) <- ask
  let t = translator config
  mrecipes <- liftIO $
    forM targets $ \target -> do
      -- Resolve the recipes all at once; this way if we remove multiple
      -- recipes based on their respective index, all of the index are
      -- resolved based on the state the book was in before we started to
      -- remove anything
      let mrecp = readRecipeRef target recipeBook
      () <- putStr $ case mrecp of
        Nothing -> target ++ t Str.doesNotExist
        Just r -> guard v *> t Str.removingRecipe ++ recipeName r ++ "...\n"
      return mrecp
  -- Remove all the resolved recipes at once
  replaceRecipeBook (recipeBook \\ catMaybes mrecipes)

remove :: [String] -> HermsReader IO ()
remove = removeWithVerbosity True

removeSilent :: [String] -> HermsReader IO ()
removeSilent = removeWithVerbosity False

shop :: [String] -> Int -> HermsReader IO ()
shop targets serv = do
  (_, recipeBook) <- ask
  let getFactor recp
        | serv == 0 = servingSize recp % 1
        | otherwise = serv % 1
  let ingrts =
        concatMap
          ( \target ->
              case readRecipeRef target recipeBook of
                Nothing -> []
                Just recp -> adjustIngredients (getFactor recp) $ ingredients recp
          )
          targets
  liftIO $
    forM_ (sort . combineIngredients $ ingrts) $ \ingr ->
      putStrLn $ showIngredient 1 ingr

printDataDir :: HermsReader IO ()
printDataDir = do
  (config, _) <- ask
  liftIO $ mapM_ (putStrLn . cleanDirPath) [dataDir config, configDir config]
  where
    cleanDirPath = (++ "/") . filter (/= '\"')

main :: IO ()
main = do
  config <- getConfig
  recipeBook <- getRecipeBookWith config
  command <- execParser (commandPI (translator config))
  case recipeBook of
    Left e -> putStrLn $ "Couldn't read recipes: " ++ show e
    Right recipeBook' ->
      runReaderT (runWithOpts command) (config, recipeBook')

-- @runWithOpts runs the action of selected command.
runWithOpts :: Command -> HermsReader IO ()
runWithOpts (List tags group nameOnly) = list tags group nameOnly
runWithOpts Add = add
runWithOpts (Edit target conversion) = edit target conversion
runWithOpts (Import target format) = importFile target format
runWithOpts (Export targets format) = export targets format
runWithOpts (Remove targets) = remove targets
runWithOpts (View targets serving step conversion) =
  if step
    then viewByStep targets serving conversion
    else view targets serving conversion
runWithOpts (Shop targets serving) = shop targets serving
runWithOpts DataDir = printDataDir
runWithOpts (Find regexp) = findRecipes regexp

------------------------------
------------ CLI -------------
------------------------------

-- | 'Command' data type represents commands of CLI
data Command
  = -- | shows recipes
    List [String] Bool Bool
  | -- | shows specified recipes with given serving
    View [String] Int Bool String
  | -- | adds the recipe (interactively)
    Add
  | -- | edits the recipe
    Edit String String
  | -- | imports a recipe file
    Import String String
  | -- | exports recipes to stdout
    Export [String] String
  | -- | removes specified recipes
    Remove [String]
  | -- | generates the shopping list for given recipes
    Shop [String] Int
  | -- | find all matching strings within recipe book (supports extended regex)
    Find String
  | -- | prints the directories of recipe file and config.hs
    DataDir

listP, addP, viewP, editP, importP, exportP, removeP, shopP, dataDirP, findP :: Translator -> Parser Command
listP t = List <$> (words <$> tagsP t) <*> groupByTagsP t <*> nameOnlyP t
addP _ = pure Add
editP t = Edit <$> recipeNameP t <*> conversionP t
importP t = Import <$> fileNameP t <*> formatP t
exportP t = Export <$> severalRecipesP t <*> formatP t
removeP t = Remove <$> severalRecipesP t
viewP t = View <$> severalRecipesP t <*> servingP t <*> stepP t <*> conversionP t
shopP t = Shop <$> severalRecipesP t <*> servingP t
findP t = Find <$> findRegexP t
dataDirP _ = pure DataDir

-- | @findRegexP@ returns a parser for a regex string
findRegexP :: Translator -> Parser String
findRegexP _ = argument str (metavar "regular expression")

-- | @groupByTagsP is flag for grouping recipes by tags
groupByTagsP :: Translator -> Parser Bool
groupByTagsP t =
  switch
    ( long (t Str.group)
        <> short Str.groupShort
        <> help (t Str.groupDesc)
    )

-- | @nameOnlyP is flag for showing recipe names only
nameOnlyP :: Translator -> Parser Bool
nameOnlyP t =
  switch
    ( long (t Str.nameOnly)
        <> short Str.nameOnlyShort
        <> help (t Str.nameOnlyDesc)
    )

-- | @tagsP returns the parser of tags
tagsP :: Translator -> Parser String
tagsP t =
  strOption
    ( long (t Str.tags)
        <> value ""
        <> metavar (t Str.tagsMetavar)
        <> help (t Str.tagsDesc)
    )

-- | @servingP returns the parser of number of servings.
servingP :: Translator -> Parser Int
servingP t =
  option
    auto
    ( long (t Str.serving)
        <> short Str.servingShort
        <> help (t Str.servingDesc)
        <> showDefault
        <> value 0
        <> metavar (t Str.servingMetavar)
    )

-- | @formatP parses import/export formats.
formatP :: Translator -> Parser String
formatP t =
  option
    str
    ( long (t Str.format)
        <> short Str.formatShort
        <> help (t Str.formatDesc)
        <> showDefault
        <> value "yaml"
        <> metavar (t Str.formatMetavar)
    )

stepP :: Translator -> Parser Bool
stepP t =
  switch
    ( long (t Str.step)
        <> short Str.stepShort
        <> help (t Str.stepDesc)
    )

-- | @recipeNameP parses the string of recipe name.
recipeNameP :: Translator -> Parser String
recipeNameP t =
  strArgument
    ( metavar (t Str.recipeNameMetavar)
        <> help (t Str.recipeNameDesc)
    )

-- | @fileNameP parses the string of a file name.
fileNameP :: Translator -> Parser String
fileNameP t =
  strArgument
    ( metavar (t Str.fileNameMetavar)
        <> help (t Str.fileNameDesc)
    )

-- | @severalRecipesP parses several recipe names at once
--   and returns the parser of list of names
severalRecipesP :: Translator -> Parser [String]
severalRecipesP t = many (recipeNameP t)

-- | @conversionP flags recipes for unit conversion
conversionP :: Translator -> Parser String
conversionP t =
  strOption
    ( long (t Str.convert)
        <> short Str.convertShort
        <> help (t Str.convertDesc)
        <> metavar (t Str.convertMetavar)
        <> value (t Str.none)
    )

-- @optP parses particular command.
optP :: Translator -> Parser Command
optP t =
  subparser $
    command
      (t Str.list)
      ( info
          (helper <*> listP t)
          (progDesc (t Str.listDesc))
      )
      <> command
        (t Str.view)
        ( info
            (helper <*> viewP t)
            (progDesc (t Str.viewDesc))
        )
      <> command
        (t Str.add)
        ( info
            (helper <*> addP t)
            (progDesc (t Str.addDesc))
        )
      <> command
        (t Str.edit)
        ( info
            (helper <*> editP t)
            (progDesc (t Str.editDesc))
        )
      <> command
        (t Str.import')
        ( info
            (helper <*> importP t)
            (progDesc (t Str.importDesc))
        )
      <> command
        (t Str.export)
        ( info
            (helper <*> exportP t)
            (progDesc (t Str.exportDesc))
        )
      <> command
        (t Str.remove)
        ( info
            (helper <*> removeP t)
            (progDesc (t Str.removeDesc))
        )
      <> command
        (t Str.shopping)
        ( info
            (helper <*> shopP t)
            (progDesc (t Str.shoppingDesc))
        )
      <> command
        (t Str.datadir)
        ( info
            (helper <*> dataDirP t)
            (progDesc (t Str.datadirDesc))
        )
      <> command
        (t Str.find)
        ( info
            (helper <*> findP t)
            (progDesc (t Str.findDesc))
        )

versionOption :: Parser (a -> a)
versionOption =
  infoOption
    versionStr
    ( long "version"
        <> short 'v'
        <> help "Show version"
    )

-- @prsr is the main parser of all CLI arguments.
commandPI :: Translator -> ParserInfo Command
commandPI t =
  info (helper <*> versionOption <*> optP t) $
    fullDesc
      <> progDesc (t Str.progDesc)
