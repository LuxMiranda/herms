{-# LANGUAGE ViewPatterns #-}

module Main where

import System.Directory
import System.IO
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Function
import Data.List
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Ratio
import Control.Applicative
import Options.Applicative hiding (str)
import Text.Read
import Utils
import AddCLI
import RichText
import Types
import UnitConversions
import ReadConfig
import Paths_herms
import Control.Exception
import GHC.IO.Exception
import Foreign.C.Error
import Lang.Base (toLang, Translator)
import qualified Lang.Strings as Str

-- Global constants
versionStr :: String
versionStr = "1.9.0.0"

configPath :: IO FilePath
configPath = getDataFileName "config.hs"

-- | @getRecipeBookWith reads in recipe book with already read-in config
getRecipeBookWith :: Config -> IO [Recipe]
getRecipeBookWith config = do
  fileName <- getDataFileName (recipesFile config)
  contents <- readFile fileName
  return $ map read $ lines contents

-- | @getRecipeBook reads in config before reading in recipe book
getRecipeBook :: IO [Recipe]
getRecipeBook = do
  config <- getConfig
  getRecipeBookWith config

getRecipe :: String -> [Recipe] -> Maybe Recipe
getRecipe target = listToMaybe . filter ((target ==) . recipeName)

saveOrDiscard :: [[String]]   -- input for the new recipe
              -> Maybe Recipe -- maybe an original recipe prior to any editing
              -> IO ()
saveOrDiscard input oldRecp = do
  let newRecipe = readRecipe input
  putTextLn $ showRecipe newRecipe Nothing
  putStrLn Str.saveRecipeYesNoEdit
  response <- getLine
  if response == Str.y || response == Str.yCap
    then do
    config <- getConfig
    recipeBook <- getRecipeBookWith config
    let recpName = maybe (recipeName newRecipe) recipeName oldRecp
    unless (isNothing (readRecipeRef recpName recipeBook)) $ removeSilent [recpName]
    fileName <- getDataFileName (recipesFile config)
    appendFile fileName (show newRecipe ++ "\n")
    putStrLn Str.recipeSaved
  else if response == Str.n || response == Str.nCap
    then
    putStrLn Str.changesDiscarded
  else if response == Str.e || response == Str.eCap
    then
    doEdit newRecipe oldRecp
  else
    do
    putStrLn ("\n" ++ Str.badEntry ++ "\n")
    saveOrDiscard input oldRecp

add :: IO ()
add = do
  input <- getAddInput
  saveOrDiscard input Nothing

doEdit :: Recipe -> Maybe Recipe -> IO ()
doEdit recp origRecp = do
  input <- getEdit (recipeName recp) (description recp) serving amounts units ingrs attrs dirs tag
  saveOrDiscard input origRecp
  where serving  = show $ servingSize recp
        ingrList = adjustIngredients (servingSize recp % 1) $ ingredients recp
        toStr f  = unlines (map f ingrList)
        amounts  = toStr (showFrac . quantity)
        units    = toStr unit
        ingrs    = toStr ingredientName
        dirs     = unlines (directions recp)
        attrs    = toStr attribute
        tag      = unlines (tags recp)

edit :: String -> IO ()
edit target = do
  recipeBook <- getRecipeBook
  case readRecipeRef target recipeBook of
    Nothing   -> putStrLn $ target ++ Str.doesNotExist
    Just recp -> doEdit recp (Just recp)
  -- Only supports editing one recipe per command

-- | `readRecipeRef target book` interprets the string `target`
--   as either an index or a recipe's name and looks up the
--   corresponding recipe in the `book`
readRecipeRef :: String -> [Recipe] -> Maybe Recipe
readRecipeRef target recipeBook =
  (safeLookup recipeBook . pred =<< readMaybe target)
  <|> getRecipe target recipeBook

importFile :: String -> IO ()
importFile target = do
  config     <- getConfig
  recipeBook <- getRecipeBookWith config
  otherRecipeBook <- map read . lines <$> readFile target
  let recipeEq = (==) `on` recipeName
  let newRecipeBook = deleteFirstsBy recipeEq recipeBook otherRecipeBook
                        ++ otherRecipeBook
  replaceDataFile (recipesFile config) $ unlines $ show <$> newRecipeBook
  if null otherRecipeBook
  then putStrLn Str.nothingToImport
  else do
    putStrLn Str.importedRecipes
    forM_ otherRecipeBook $ \recipe ->
      putStrLn $ "  " ++ recipeName recipe

getServingsAndConv :: Int -> String -> Config -> (Maybe Int, Conversion)
getServingsAndConv serv convName config = (servings, conv)
  where servings = case serv of
                   0 -> case defaultServingSize config of
                           0 -> Nothing
                           j -> Just j
                   i -> Just i
        conv
          | convName  == Str.metric   =  Metric
          | convName  == Str.imperial =  Imperial
          | otherwise = defaultUnit config

view :: [String] -> Int -> String -> ReaderT (Config, RecipeBook) IO ()
view targets serv convName = do
  (config,recipeBook) <- ask
  let (servings, conv) = getServingsAndConv serv convName config
  liftIO $ forM_ targets $ \ target ->
    putText $ case readRecipeRef target recipeBook of
      Nothing   -> target ~~ Str.doesNotExist
      Just recp -> showRecipe (convertRecipeUnits conv recp) servings

viewByStep :: [String] -> Int -> String -> IO ()
viewByStep targets serv convName = do
  config     <- getConfig
  recipeBook <- getRecipeBookWith config
  let (servings, conv) = getServingsAndConv serv convName config
  hSetBuffering stdout NoBuffering
  forM_ targets $ \ target -> case readRecipeRef target recipeBook of
    Nothing   -> putStr $ target ++ Str.doesNotExist
    Just recp -> viewRecipeByStep (convertRecipeUnits conv recp) servings

viewRecipeByStep :: Recipe -> Maybe Int -> IO ()
viewRecipeByStep recp servings = do
  putText $ showRecipeHeader recp servings
  let steps = showRecipeSteps recp
  forM_ (init steps) $ \ step -> do
    putStr $ step ++ Str.more
    getLine
  putStr $ last steps ++ "\n"

list :: [String] -> Bool -> Bool -> ReaderT (Config, RecipeBook) IO ()
list inputTags groupByTags nameOnly = do
  (config,recipes) <- ask
  let recipesWithIndex = zip [1..] recipes
  let targetRecipes    = filterByTags inputTags recipesWithIndex
  liftIO $ if groupByTags
  then listByTags nameOnly inputTags targetRecipes
  else listDefault nameOnly targetRecipes

filterByTags :: [String] -> [(Int, Recipe)] -> [(Int, Recipe)]
filterByTags []        = id
filterByTags inputTags = filter (inTags . tags . snd)
 where
  inTags r = all (`elem` r) inputTags

listDefault :: Bool -> [(Int, Recipe)] -> IO ()
listDefault nameOnly (unzip -> (indices, recipes)) = do
  let recipeList = map showRecipeInfo recipes
      size       = length $ show $ length recipeList
      strIndices = map (padLeft size . show) indices
  if nameOnly
  then mapM_ (putStrLn . recipeName) recipes
  else mapM_ putTextLn $ zipWith (\ i -> ((i ~~ ". ") ~~)) strIndices recipeList

listByTags :: Bool -> [String] -> [(Int, Recipe)] -> IO ()
listByTags nameOnly inputTags recipesWithIdx = do
  let tagsRecipes :: [[(String, (Int, Recipe))]]
      tagsRecipes =
        groupBy ((==) `on` fst) $ sortBy (compare `on` fst) $
          concat $ flip map recipesWithIdx $ \recipeWithIdx ->
            map (flip (,) recipeWithIdx) $ tags $ snd recipeWithIdx
  forM_ tagsRecipes $ \tagRecipes -> do
    putTextLn $ bold $ fontColor Magenta $ fst $ head tagRecipes -- Tag name
    forM_ (map snd tagRecipes) $ \(i, recipe) ->
      if nameOnly
      then putStrLn $ recipeName recipe
      else putTextLn $ "  " ~~ show i ~~ ". " ~~ showRecipeInfo recipe
    putStrLn ""

showRecipeInfo :: Recipe -> RichText
showRecipeInfo recipe = name ~~ "\n\t" ~~ desc ~~ "\n\t[" ~~ tagsStr ~~ ": " ~~ showTags ~~ "]"
  where name     = fontColor Blue $ recipeName recipe
        desc     = (takeFullWords . description) recipe
        showTags = fontColor Green $ (intercalate ", " . tags) recipe
        tagsStr  = fontColor White $ Str.capTags

takeFullWords :: String -> String
takeFullWords = unwords . takeFullWords' 0 . words
  where takeFullWords' _ []                           = []
        takeFullWords' n [x]    | (length x + n) > 40 = []
                                | otherwise           = [x]
        takeFullWords' n (x:xs) | (length x + n) > 40 = [x ++ "..."]
                                | otherwise           =
                                  x : takeFullWords' (length x + n) xs

-- | @replaceDataFile fp str@ replaces the target data file @fp@ with
--   the new content @str@ in a safe manner: it opens a temporary file
--   first, writes to it, closes the handle, removes the target,
--   and finally moves the temporary file over to the target @fp@.

replaceDataFile :: FilePath -> String -> IO ()
replaceDataFile fp str = do
  (tempName, tempHandle) <- openTempFile "." "herms_temp"
  hPutStr tempHandle str
  hClose tempHandle
  fileName <- getDataFileName fp
  removeFile fileName
  let exdev e = if ioe_errno e == Just ((\(Errno a) -> a) eXDEV)
                    then copyFile tempName fileName >> removeFile tempName
                    else throw e
  renameFile tempName fileName `catch` exdev

-- | @removeWithVerbosity v recipes@ deletes the @recipes@ from the
--   book, listing its work only if @v@ is set to @True@.
--   This subsumes both @remove@ and @removeSilent@.

removeWithVerbosity :: Bool -> [String] -> IO ()
removeWithVerbosity v targets = do
  config     <- getConfig
  recipeBook <- getRecipeBookWith config
  mrecipes   <- forM targets $ \ target -> do
    -- Resolve the recipes all at once; this way if we remove multiple
    -- recipes based on their respective index, all of the index are
    -- resolved based on the state the book was in before we started to
    -- remove anything
    let mrecp = readRecipeRef target recipeBook
    () <- putStr $ case mrecp of
       Nothing -> target ++ Str.doesNotExist
       Just r  -> guard v *> Str.removingRecipe ++ recipeName r ++ "...\n"
    return mrecp
  -- Remove all the resolved recipes at once
  let newRecipeBook = recipeBook \\ catMaybes mrecipes
  replaceDataFile (recipesFile config) $ unlines $ show <$> newRecipeBook

remove :: [String] -> IO ()
remove = removeWithVerbosity True

removeSilent :: [String] -> IO ()
removeSilent = removeWithVerbosity False


shop :: [String] -> Int -> IO ()
shop targets serv = do
  recipeBook <- getRecipeBook
  let getFactor recp
        | serv == 0 = servingSize recp % 1
        | otherwise = serv % 1
  let ingrts =
        concatMap (\target ->
          case readRecipeRef target recipeBook of
            Nothing   -> []
            Just recp -> adjustIngredients (getFactor recp) $ ingredients recp)
                  targets
  forM_ (sort ingrts) $ \ingr ->
    putStrLn $ showIngredient 1 ingr

printDataDir :: IO ()
printDataDir = do
  dir <- getDataDir
  putStrLn $ filter (/= '\"') (show dir) ++ "/"

-- Writes an empty recipes file if it doesn't exist
checkFileExists :: IO ()
checkFileExists = do
  config   <- getConfig
  fileName <- getDataFileName (recipesFile config)
  fileExists <- doesFileExist fileName
  unless fileExists (do
    dirName <- getDataDir
    createDirectoryIfMissing True dirName
    writeFile fileName "")

main = do
  config <- getConfig
  recipeBook <- getRecipeBookWith config
--  execParser commandPI >>= runWithOpts
--  runReaderT (runWithOpts $ View ["1"] 1 False "metric") (config, recipeBook)
  command <- execParser commandPI
  runReaderT (runWithOpts command) (config, recipeBook)

-- @runWithOpts runs the action of selected command.
runWithOpts :: Command -> ReaderT (Config, RecipeBook) IO ()
runWithOpts (List tags group nameOnly)              = list tags group nameOnly
--runWithOpts Add                                     = add
--runWithOpts (Edit target)                           = edit target
--runWithOpts (Import target)                         = importFile target
--runWithOpts (Remove targets)                        = remove targets
runWithOpts (View targets serving step conversion)  = view targets serving conversion{--if step then viewByStep targets serving conversion
                                                                     else view targets serving conversion config --}
--runWithOpts (Shop targets serving)                  = shop targets serving
--runWithOpts DataDir                                 = printDataDir


------------------------------
------------ CLI -------------
------------------------------

-- | 'Command' data type represents commands of CLI
data Command = List   [String] Bool Bool         -- ^ shows recipes
             | View   [String] Int Bool String   -- ^ shows specified recipes with given serving
{--
             | Add                               -- ^ adds the recipe (interactively)
             | Edit    String                    -- ^ edits the recipe
             | Import  String                    -- ^ imports a recipe file
             | Remove [String]                   -- ^ removes specified recipes
             | Shop   [String] Int               -- ^ generates the shopping list for given recipes
             | DataDir                           -- ^ prints the directory of recipe file and config.hs

--}

--listP, addP, editP, removeP, viewP, shopP, dataDirP :: Parser Command
listP,viewP :: Parser Command
listP    = List   <$> (words <$> tagsP) <*> groupByTagsP <*> nameOnlyP
--addP     = pure Add
--editP    = Edit   <$> recipeNameP
--importP  = Import <$> fileNameP
--removeP  = Remove <$> severalRecipesP
viewP    = View   <$> severalRecipesP <*> servingP <*> stepP <*> conversionP
--shopP    = Shop   <$> severalRecipesP <*> servingP
--dataDirP = pure DataDir


-- | @groupByTagsP is flag for grouping recipes by tags
groupByTagsP :: Parser Bool
groupByTagsP = switch
         (  long Str.group
         <> short Str.groupShort
         <> help Str.groupDesc
         )

-- | @nameOnlyP is flag for showing recipe names only
nameOnlyP :: Parser Bool
nameOnlyP = switch
         (  long Str.nameOnly
         <> short Str.nameOnlyShort
         <> help Str.nameOnlyDesc
         )

-- | @tagsP returns the parser of tags
tagsP :: Parser String
tagsP = strOption (  long Str.tags
                  <> value ""
                  <> metavar Str.tagsMetavar
                  <> help Str.tagsDesc
                  )

-- | @servingP returns the parser of number of servings.
servingP :: Parser Int
servingP =  option auto
         (  long Str.serving
         <> short Str.servingShort
         <> help Str.servingDesc
         <> showDefault
         <> value 0
         <> metavar Str.servingMetavar )

stepP :: Parser Bool
stepP = switch
    (  long Str.step
    <> short Str.stepShort
    <> help Str.stepDesc )

-- | @recipeNameP parses the string of recipe name.
recipeNameP :: Parser String
recipeNameP = strArgument (  metavar Str.recipeNameMetavar
                          <> help Str.recipeNameDesc)

-- | @fileNameP parses the string of a file name.
fileNameP :: Parser String
fileNameP = strArgument (  metavar Str.fileNameMetavar
                        <> help Str.fileNameDesc)

-- | @severalRecipesP parses several recipe names at once
--   and returns the parser of list of names
severalRecipesP :: Parser [String]
severalRecipesP = many recipeNameP

-- | @conversionP flags recipes for unit conversion
conversionP :: Parser String
conversionP = strOption
            (long Str.convert
            <> short Str.convertShort
            <> help Str.convertDesc
            <> metavar Str.convertMetavar
            <> value Str.none)

-- @optP parses particular command.
optP :: Parser Command
optP = subparser
     $  command Str.list (info (helper <*> listP)
                      (progDesc (Str.listDesc)))
     <> command Str.view
                (info  (helper <*> viewP)
                       (progDesc (Str.viewDesc)))
    {-- <> command Str.add
                (info  (helper <*> addP)
                       (progDesc (translate Str.addDesc)))
     <> command Str.edit
                (info  (helper <*> editP)
                       (progDesc (translate Str.editDesc)))
     <> command Str.import'
                (info  (helper <*> importP)
                       (progDesc (translate Str.importDesc)))
     <> command Str.remove
                (info  (helper <*> removeP)
                       (progDesc (translate Str.removeDesc)))
     <> command Str.shopping
                (info (helper <*> shopP)
                      (progDesc (translate Str.shoppingDesc)))
     <> command Str.datadir
                (info (helper <*> dataDirP)
                      (progDesc (translate Str.datadirDesc)))
                --}

versionOption :: Parser (a -> a)
versionOption = infoOption versionStr
                (long Str.version
                <> short Str.versionShort
                <> help Str.versionDesc)

-- @prsr is the main parser of all CLI arguments.
commandPI :: ParserInfo Command
commandPI = info ( helper <*> versionOption <*> (optP) )
          $  fullDesc
          <> progDesc (Str.progDesc)
