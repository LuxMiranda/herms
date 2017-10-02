module Main where

import System.Directory
import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.Semigroup ((<>))
import Control.Applicative
import Options.Applicative hiding (str)
import Text.Read
import Utils
import AddCLI
import Types
import Paths_herms

-- Global constant
recipesFileName :: String
recipesFileName = "recipes.herms"

getRecipeBook :: IO [Recipe]
getRecipeBook = do
  fileName <- getDataFileName recipesFileName
  contents <- readFile fileName
  return $ map read $ lines contents

getRecipe :: String -> [Recipe] -> Maybe Recipe
getRecipe target = listToMaybe . filter ((target ==) . recipeName)

saveOrDiscard :: [[String]]   -- input for the new recipe
              -> Maybe Recipe -- maybe an original recipe prior to any editing
              -> IO ()
saveOrDiscard input oldRecp = do
  let newRecipe = readRecipe input
  putStrLn $ showRecipe newRecipe Nothing
  putStrLn "Save recipe? (Y)es  (N)o  (E)dit"
  response <- getLine
  if response == "y" || response == "Y"
    then do
    recipeBook <- getRecipeBook
    let recpName = recipeName (fromJust oldRecp)
    unless (isNothing (readRecipeRef recpName recipeBook)) $ removeSilent [recpName]
    fileName <- getDataFileName recipesFileName
    appendFile fileName (show newRecipe ++ "\n")
    putStrLn "Recipe saved!"
  else if response == "n" || response == "N"
    then do
    putStrLn "Recipe discarded."
  else if response == "e" || response == "E"
    then do
    doEdit newRecipe oldRecp
  else
    do
    putStrLn "\nPlease enter ONLY 'y', 'n' or 'e'\n"
    saveOrDiscard input oldRecp

add :: IO ()
add = do
  input <- getAddInput
  saveOrDiscard input Nothing

doEdit :: Recipe -> Maybe Recipe -> IO ()
doEdit recp origRecp = do
  input <- getEdit (recipeName recp) (description recp) amounts units ingrs attrs dirs tag
  saveOrDiscard input origRecp
  where ingrList = ingredients recp
        toStr    = (\ f -> unlines (map f ingrList))
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
    Nothing   -> putStrLn $ target ++ " does not exist\n"
    Just recp -> doEdit recp (Just recp)
  -- Only supports editing one recipe per command

-- | `readRecipeRef target book` interprets the string `target`
--   as either an index or a recipe's name and looks up the
--   corresponding recipe in the `book`
readRecipeRef :: String -> [Recipe] -> Maybe Recipe
readRecipeRef target recipeBook =
  (safeLookup recipeBook . pred =<< readMaybe target)
  <|> getRecipe target recipeBook

view :: [String] -> Int -> IO ()
view targets serv = do
  recipeBook <- getRecipeBook
  let servings = case serv of
                   0 -> Nothing
                   i -> Just i
  forM_ targets $ \ target ->
    putStr $ case readRecipeRef target recipeBook of
      Nothing   -> target ++ " does not exist\n"
      Just recp -> showRecipe recp servings

list :: IO ()
list = do
  recipes <- getRecipeBook
  let recipeList = map showRecipeInfo recipes
      size       = length $ show $ length recipeList
      indices    = map (padLeft size . show) [1..]
  putStr $ unlines $ zipWith (\ i -> ((i ++ ". ") ++)) indices recipeList


showRecipeInfo :: Recipe -> String
showRecipeInfo recipe = name ++ "\n\t" ++ desc  ++ "\n\t[Tags: " ++ showTags ++ "]"
  where name     = recipeName recipe
        desc     = (takeFullWords . description) recipe
        showTags = (intercalate ", " . tags) recipe

takeFullWords :: String -> String
takeFullWords = (unwords . takeFullWords' 0 . words)
  where takeFullWords' n (x:[]) | (length x + n) > 40 = []
                                | otherwise           = [x]
        takeFullWords' n (x:xs) | (length x + n) > 40 = [x ++ "..."]
                                | otherwise           =
                                  [x] ++ takeFullWords' ((length x) + n) xs

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
  renameFile tempName fileName

-- | @removeWithVerbosity v recipes@ deletes the @recipes@ from the
--   book, listing its work only if @v@ is set to @True@.
--   This subsumes both @remove@ and @removeSilent@.

removeWithVerbosity :: Bool -> [String] -> IO ()
removeWithVerbosity v targets = do
  recipeBook <- getRecipeBook
  mrecipes   <- forM targets $ \ target -> do
    -- Resolve the recipes all at once; this way if we remove multiple
    -- recipes based on their respective index, all of the index are
    -- resolved based on the state the book was in before we started to
    -- remove anything
    let mrecp = readRecipeRef target recipeBook
    () <- putStr $ case mrecp of
       Nothing -> target ++ " does not exist.\n"
       Just r  -> guard v *> "Removing recipe: " ++ recipeName r ++ "...\n"
    return mrecp
  -- Remove all the resolved recipes at once
  let newRecipeBook = recipeBook \\ catMaybes mrecipes
  replaceDataFile recipesFileName $ unlines $ show <$> newRecipeBook

remove :: [String] -> IO ()
remove = removeWithVerbosity True

removeSilent :: [String] -> IO ()
removeSilent = removeWithVerbosity False


shop :: [String] -> Int -> IO ()
shop targets serv = do
  recipeBook <- getRecipeBook
  let servings = case serv of
                   0 -> Nothing
                   i -> Just i
  let ingrts =
        concatMap (\target ->
          case readRecipeRef target recipeBook of
            Nothing   -> []
            Just recp -> ingredients recp)
                  targets
  forM_ (sort ingrts) $ \ingr ->
    putStrLn $ showIngredient servings ingr

-- Writes an empty recipes file if it doesn't exist
checkFileExists :: IO ()
checkFileExists = do
  fileName <- getDataFileName recipesFileName
  fileExists <- doesFileExist fileName
  unless fileExists (do
    dirName <- getDataDir
    createDirectoryIfMissing True dirName
    writeFile fileName "")

main :: IO ()
main = execParser commandPI >>= runWithOpts

-- @runWithOpts runs the action of selected command.
runWithOpts :: Command -> IO ()
runWithOpts List                   = list
runWithOpts Add                    = add
runWithOpts (Edit target)          = edit target
runWithOpts (Remove targets)       = remove targets
runWithOpts (View targets serving) = view targets serving
runWithOpts (Shop targets serving) = shop targets serving

------------------------------
------------ CLI -------------
------------------------------

-- | 'Command' data type represents commands of CLI
data Command = List                -- ^ shows all recipes
             | Add                 -- ^ adds the recipe (interactively)
             | Edit    String      -- ^ edits the recipe
             | Remove [String]     -- ^ removes specified recipes
             | View   [String] Int -- ^ shows specified recipes with given serving
             | Shop   [String] Int -- ^ generates the shopping list for given recipes

listP, addP, editP, removeP, viewP, shopP :: Parser Command
listP   = pure List
addP    = pure Add
editP   = Edit   <$> recipeNameP
removeP = Remove <$> severalRecipesP
viewP   = View   <$> severalRecipesP <*> servingP
shopP   = Shop   <$> severalRecipesP <*> servingP

-- | @servingP returns the parser of number of servings.
servingP :: Parser Int
servingP =  option auto
         (  long "serving"
         <> short 's'
         <> help "specify serving size when viewing"
         <> showDefault
         <> value 0
         <> metavar "INT" )

-- | @recipeNameP parses the string of recipe name.
recipeNameP :: Parser String
recipeNameP = strArgument (  metavar "RECIPE_NAME"
                          <> help "index or Recipe name")

-- | @severalRecipesP parses several recipe names at once
--   and returns the parser of list of names
severalRecipesP :: Parser [String]
severalRecipesP = many recipeNameP

-- @optP parses particular command.
optP :: Parser Command
optP =  subparser
     $  command "list"
                (info (helper <*> listP)
                      (progDesc "list recipes"))
     <> command "view"
                (info  (helper <*> viewP)
                       (progDesc "view the particular recipes"))
     <> command "add"
                (info  (helper <*> addP)
                       (progDesc "add a new recipe (interactively)"))
     <> command "edit"
                (info  (helper <*> editP)
                       (progDesc "edit a recipe"))
     <> command "remove"
                (info  (helper <*> removeP)
                       (progDesc "remove the particular recipes"))
     <> command "shopping"
                (info (helper <*> shopP)
                      (progDesc "generate a shopping list for given recipes"))

-- @prsr is the main parser of all CLI arguments.
commandPI :: ParserInfo Command
commandPI =  info ( helper <*> optP )
          $  fullDesc
          <> progDesc "HeRM's: a Haskell-based Recipe Manager"
