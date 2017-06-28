import System.Environment
import System.Directory
import System.IO
import Data.Char

--TODOS:
-- Make printing recipes pretty
-- Create a function that handles file-reading so we can stay DRY
-- Implemet the other stuff

-- Global constant
fileName = "recipes"

data Ingredient = Ingredient { quantity :: Double
                             , unit :: String
                             , ingredientName :: String
                             , attribute :: String
                             } deriving (Eq, Show, Read)

data Recipe = Recipe { recipeName :: String
                     , ingredients :: [Ingredient]
                     , directions :: [String]
                     , tags :: [String]
                     } deriving (Eq, Show, Read)


getRecipe :: String -> [Recipe] -> Maybe Recipe
getRecipe _ [] = Nothing
getRecipe target (r:book) = if recipeName r == target 
                              then Just r 
                            else 
                              getRecipe target book

-- TODO
add :: [String] -> IO ()
add _ = print $ "I am adding!"

showIngredient :: Ingredient -> String
showIngredient i
  | quantity i == 0 && attribute i == "" = ingredientName i
  | quantity i == 0 = ingredientName i ++ ", " ++ attribute i
  | attribute i == "" = (show $ quantity i) ++ " " ++ unit i ++ " " ++ ingredientName i
  | otherwise = (show $ quantity i) ++ " " ++ unit i ++ " " ++ ingredientName i ++ ", " ++ attribute i

showRecipe :: Recipe -> String
showRecipe r =  (map toUpper $ recipeName r) ++ "\n"
                ++ unlines (map showIngredient $ ingredients r)
                ++ unlines (directions r)

view :: [String] -> IO ()
view [target] = do
  contents <- readFile fileName
  let recipesStr = lines contents
      recipeBook = map (read::String->Recipe) recipesStr
      (Just recp)= getRecipe target recipeBook
  putStr $ showRecipe recp

list :: [String] -> IO ()
list _  = do
  contents <- readFile fileName
  let recipesStr = lines contents
      recipes    = map (read::String->Recipe) recipesStr
      recipeList = map recipeName recipes
  putStr $ unlines recipeList

-- TODO
remove :: [String] -> IO ()
remove _ = print $ "I am removing!"

help :: [String] -> IO ()
help _ = putStr $ unlines [ "Usage:"
                         , "./herms list                  - list recipes"
                         , "./herms view \"Recipe Name\"    - view a particular recipe"
                         , "./herms add                   - add a new recipe (interactive)"
                         , "./herms remove \"Recipe Name\"  - remove a particular recipe"
                         , "./herms help                  - display this help"
                         ]

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
           , ("view", view)
           , ("remove", remove)
           , ("list", list)
           , ("help", help)
           ]

main = do
  testCmd <- getArgs
  if testCmd == [] 
    then help [""]
  else do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args
