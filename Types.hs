module Herms.Types where

import Data.List.Split
import Data.Ratio

data Ingredient = Ingredient { quantity :: Ratio Int
                             , unit :: String
                             , ingredientName :: String
                             , attribute :: String
                             } deriving (Eq, Show, Read)

data Recipe = Recipe { recipeName :: String
                     , description :: String
                     , ingredients :: [Ingredient]
                     , directions :: [String]
                     , tags :: [String]
                     } deriving (Eq, Show, Read)

showFrac :: Ratio Int -> String
showFrac x
  | numerator x == denominator x = show (numerator x)
  | denominator x == 1 = show (numerator x)
  | whole > 0 = show whole ++ " " ++  showFrac (x - (fromIntegral whole))
  | otherwise = show (numerator x) ++ "/" ++ show (denominator x)
  where whole = floor $ fromIntegral (numerator x) / fromIntegral (denominator x)

readFrac :: String -> Ratio Int
readFrac x
  | ' ' `elem` x = let xs = splitOn " " x in ((read' (head xs)) % 1) + (readFrac (last xs))
  | '/' `elem` x = let xs = splitOn "/" x in (read' (head xs)) % (read' (last xs))
  | otherwise = (read x :: Int) % 1
  where read' = read :: String -> Int

showIngredient :: Ingredient -> String
showIngredient i = qty ++ u ++ (ingredientName i) ++ att
  where qty = if quantity i == 0 then "" else showFrac (quantity i) ++ " "
        u   = if null (unit i) then "" else (unit i) ++ " "
        att = if null (attribute i) then "" else ", " ++ (attribute i)

showRecipe :: Recipe -> String
showRecipe r =  "+--" ++ filler ++ "+\n"
                ++ "|  " ++ recipeName r ++ "  |\n"
                ++ "+--" ++ filler ++ "+\n"
                ++ "\n" ++ description r ++ "\n"
                ++ "\nIngredients:\n"
                ++ unlines (zipWith (\a b -> a ++ b) (repeat "* ") $ map showIngredient $ ingredients r)
                ++ "\n" ++ unlines (zipWith (\i d -> "(" ++ show i ++ ") " ++ d) [1..] (directions r))
                where filler = take ((length $ recipeName r) + 2) $ repeat '-'


