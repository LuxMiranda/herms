module Main where

import System.Environment
import System.Directory
import System.IO
import Control.Monad
import Data.Char
import Data.Ratio
import Data.List
import Data.Maybe
import Control.Applicative
import Text.Read
import Utils
import AddCLI
import Types
import Paths_herms

-- Global constant
recipesFileName = "recipes.herms"

getRecipeBook :: IO [Recipe]
getRecipeBook = do
  fileName <- getDataFileName recipesFileName
  contents <- readFile fileName
  return $ map read $ lines contents

getRecipe :: String -> [Recipe] -> Maybe Recipe
getRecipe target = listToMaybe . filter ((target ==) . recipeName)

add :: [String] -> IO ()
add _ = do
  input <- getAddInput 
  let newRecipe = readRecipe input
  putStrLn $ showRecipe newRecipe Nothing
  putStrLn "Save recipe? (Y)es  (N)o"
  response <- getLine
  if response == "y" || response == "Y" 
    then do 
    fileName <- getDataFileName recipesFileName
    appendFile fileName (show newRecipe ++ "\n")
    putStrLn "Recipe saved!"
  else
    putStrLn "Recipe discarded."

-- | `readRecipeRef target book` interprets the string `target`
--   as either an index or a recipe's name and looks up the
--   corresponding recipe in the `book`
readRecipeRef :: String -> [Recipe] -> Maybe Recipe
readRecipeRef target recipeBook =
  (safeLookup recipeBook . pred =<< readMaybe target)
  <|> getRecipe target recipeBook

sepFlags :: [String] -> [String]
sepFlags args = takeWhile (\x -> head x == '-') (sort args)

view :: [String] -> IO ()
view args = do
  recipeBook <- getRecipeBook
  let flags   = sepFlags args
  let cFlags  = concat flags
  let targets = args \\ flags
  let servings = case elemIndex 's' cFlags of
                   Nothing -> Nothing
                   Just i  -> Just (digitToInt (cFlags !! 2))
  forM_ targets $ \ target ->
    putStr $ case readRecipeRef target recipeBook of
      Nothing   -> target ++ " does not exist\n"
      Just recp -> showRecipe recp servings

list :: [String] -> IO ()
list _  = do
  recipes <- getRecipeBook
  let recipeList = map recipeName recipes
      size       = length $ show $ length recipeList
      indices    = map (padLeft size . show) [1..]
  putStr $ unlines $ zipWith (\ i -> ((i ++ ". ") ++)) indices recipeList

remove :: [String] -> IO ()
remove targets = forM_ targets $ \ target -> do
  recipeBook <- getRecipeBook
  (tempName, tempHandle) <- openTempFile "." "herms_temp"
  case readRecipeRef target recipeBook of
    Nothing   -> putStrLn $ target ++ " does not exist\n"
    Just recp -> do
      let newRecpBook = delete recp recipeBook
      putStrLn $ "Removing recipe: " ++ recipeName recp ++ "..."
      hPutStr tempHandle $ unlines $ show <$> newRecpBook
      putStrLn "Recipe deleted."
  hClose tempHandle
  fileName <- getDataFileName recipesFileName
  removeFile fileName
  renameFile tempName fileName

help :: [String] -> IO ()
help _ = putStr $ unlines $ "Usage:" : usage where

  usage = map (\ (c, d) -> concat [ padRight size c, "   ", d ]) desc
  size  = maximum $ map (length . fst) desc
  desc  = [ ("\therms list", "list recipes")
          , ("\therms view {index or \"Recipe Name\"}", "view a particular recipe")
          , ("\therms add", "add a new recipe (interactive)")
          , ("\therms remove {index or \"Recipe Name\"}", "remove a particular recipe")
          , ("\therms help", "display this help")
          , ("OPTIONS","")
          , ("\t-s{num}", "specify serving size when viewing.")
          , ("\t","E.g., 'herms view -s2 {recipe}' for two servings")
          ]

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
           , ("view", view)
           , ("remove", remove)
           , ("list", list)
           , ("help", help)
           ]

-- Writes an empty recipes file if it doesn't exist
checkFileExists :: IO ()
checkFileExists = do
  fileName <- getDataFileName recipesFileName
  fileExists <- doesFileExist fileName
  unless fileExists (do
    dirName <- getDataDir
    createDirectoryIfMissing True dirName
    writeFile fileName "")

herms :: [String]      -- command line arguments
      -> Maybe (IO ()) -- failure or resulting IO action
herms args = do
  guard (not $ null args)
  action <- lookup (head args) dispatch
  return $ action (tail args)

main :: IO ()
main = do
  checkFileExists
  testCmd <- getArgs
  fromMaybe (help [""]) (herms testCmd)
