import System.Environment
import System.Directory
import System.IO
import Data.Char

data Ingredient = Ingredient { quantity :: Double
                             , unit :: String
                             , ingredientName :: String
                             , attribute :: String
                             } deriving (Eq, Show, Read)

--instance Show Ingredient where
--  show a = show (quantity a) ++ " " ++ unit a ++ " " ++ name a ++ ", " ++ attribute a
-- Use pattern matching for things that lack quantities and attributes
-- Maybe keep show but have another function for nice display

data Recipe = Recipe { recipeName :: String
                     , ingredients :: [Ingredient]
                     , directions :: [String]
                     , tags :: [String]
                     } deriving (Eq, Show, Read)


getRecipe :: String -> [Recipe] -> Maybe Recipe
getRecipe _ [] = Nothing
getRecipe target (r:book) = if recipeName r == target then Just r else getRecipe target book

add :: [String] -> IO ()
add _ = print $ "I am adding!"

view :: [String] -> IO ()
view ["list"]   = do
  contents <- readFile "recipes"
  let recipesStr = lines contents
      recipes    = map (read::String->Recipe) recipesStr
      recipeList = map recipeName recipes
  putStr $ unlines recipeList
view [target] = do
  contents <- readFile "recipes"
  let recipesStr = lines contents
      recipeBook = map (read::String->Recipe) recipesStr
  putStr $ show $ getRecipe target recipeBook
  

remove :: [String] -> IO ()
remove _ = print $ "I am removing!"

-- Convert list into its own command
-- Also add usage for no given arguments
dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
           , ("view", view)
           , ("remove", remove)
           ]

main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args
