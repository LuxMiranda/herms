{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics
import Data.Char (toLower)
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ratio
import Data.Ord
import RichText
import qualified Lang.Strings as Str
import Text.Read (readMaybe)

data Unit =
  -- Imperial
    Tsp
  | Tbsp
  | Cup
  | Oz
  | FlOz
  | Lb
  | Pint
  | Quart
  | Gallon
  -- Metric
  | Ml -- deceptively, this is milliliters (mL)
  | L  -- liters (L)
  | G  -- grams (g)
  -- Other/unknown
  | Other String
  deriving (Eq, Generic, Read, Show, Ord)

-- We don't make this the default "Show" implementation so that
-- GHC can derive Show and Read instances such that
-- (read (show u)) == u
showUnit :: Unit -> String
showUnit u =
  case u of
    -- Imperial
    Tsp       -> "tsp"
    Tbsp      -> "Tbsp"
    Cup       -> "cup"
    Oz        -> "oz"
    FlOz      -> "fl oz"
    Lb        -> "lb"
    Pint      -> "pint"
    Quart     -> "quart"
    Gallon    -> "gallon"
    -- Metric
    Ml        -> "mL"
    L         -> "L"
    G         -> "g"
    Other str -> str

parseUnit :: String -> Unit
parseUnit units
  | u `elem` tspSyns  = Tsp
  | u `elem` tbspSyns = Tbsp
  | u `elem` cupSyns  = Cup
  | u `elem` ozSyns   = Oz
  | u `elem` flOzSyns = FlOz
  | u `elem` lbsSyns  = Lb
  | u `elem` pintSyns = Pint
  | u `elem` quartSyns= Quart
  | u `elem` galSyns  = Gallon
  | otherwise         = Other u
  where u = map toLower units
        tspSyns  = [ "tsp",
                     "tsp.",
                     "teaspoon",
                     "teaspoons" ]

        tbspSyns = [ "tbsp",
                     "tbsp.",
                     "tablespoon",
                     "tablespoons" ]

        cupSyns  = [ "cup",
                     "cups",
                     "cp",
                     "cps",
                     "cp." ,
                     "cps." ]

        ozSyns   = [ "oz",
                     "oz.",
                     "ounce",
                     "ounces" ]

        flOzSyns = [ "fl oz",
                     "fl. oz.",
                     "fl oz.",
                     "fl. oz",
                     "fluid ounce",
                     "fluid ounces",
                     "fluid oz",
                     "fluid oz.",
                     "fl ounce",
                     "fl ounces",
                     "fl. ounce",
                     "fl. ounces" ]

        lbsSyns =  [ "lb.",
                     "lbs.",
                     "lb",
                     "lbs",
                     "pound",
                     "pounds" ]

        pintSyns = [ "pt",
                     "pt.",
                     "pts",
                     "pts.",
                     "pint",
                     "pints" ]

        quartSyns = ["qt",
                     "qt.",
                     "qts",
                     "qts.",
                     "quart",
                     "quarts" ]

        galSyns  = [ "gal",
                     "gal.",
                     "gals",
                     "gals.",
                     "gallon",
                     "gallons" ]

data Ingredient = Ingredient { quantity       :: Ratio Int
                             , unit           :: Unit
                             , ingredientName :: String
                             , attribute      :: String
                             } deriving (Generic, Show, Read)

instance Eq Ingredient where
  ingr1 == ingr2 =  ingredientName ingr1 == ingredientName ingr2
                 && unit ingr1 == unit ingr2

instance Ord Ingredient where
  ingr1 `compare` ingr2 =
    case ingredientName ingr1 `compare` ingredientName ingr2 of
      EQ -> case unit ingr1 `compare` unit ingr2 of
              EQ -> case attribute ingr1 `compare` attribute ingr2 of
                      EQ -> quantity ingr1 `compare` quantity ingr2
                      x  -> x
              x  -> x
      x  -> x

data Recipe = Recipe { recipeName :: String
                     , description :: String
                     , servingSize :: Int
                     , ingredients :: [Ingredient]
                     , directions :: [String]
                     , tags :: [String]
                     } deriving (Eq, Generic, Show, Read)

type RecipeBook = [Recipe]

-- | @showFrac displays improper fractions
--
-- >>> showFrac (2 / 3)
-- "2/3"
--
-- >>> showFrac (5 / 4)
-- "1 1/4"
--
showFrac :: Ratio Int -> String
showFrac x
  | numerator x == denominator x = show (numerator x)
  | denominator x == 1 = show (numerator x)
  | whole > 0 = show whole ++ " " ++  showFrac (x - fromIntegral whole)
  | otherwise = show (numerator x) ++ "/" ++ show (denominator x)
  where whole = floor $ fromIntegral (numerator x) / fromIntegral (denominator x)

readFrac :: String -> Ratio Int
readFrac x
  | ' ' `elem` x = let xs = splitOn " " x in (read' (head xs) % 1) + readFrac (last xs)
  | '/' `elem` x = let xs = splitOn "/" x in read' (head xs) % read' (last xs)
  | otherwise = (read x :: Int) % 1
  where read' = read :: String -> Int

showIngredient :: Int -> Ingredient -> String
showIngredient servings i = qty ++ show (unit i) ++ ingredientName i ++ att
  where qty = if quantity i == 0
              then ""
              else showFrac (quantity i * (servings % 1)) ++ " "
        att = if null (attribute i) then "" else ", " ++ attribute i

-- TODO: rewrite as a `fold`
makeIngredients :: [[String]] -> [Ingredient]
makeIngredients [] = []
makeIngredients (i:is) = Ingredient { quantity       = q
                                    , unit           = parseUnit (i !! 1)
                                    , ingredientName = i !! 2
                                    , attribute      = i !! 3
                                    } : makeIngredients is
                                    where q = if null (head i) then 0 % 1 else readFrac (head i)


readIngredients :: [[String]] -> [Ingredient]
readIngredients is = makeIngredients $ transpose $ fillVoid is (length (maximumBy (comparing length) is))

adjustIngredients :: Ratio Int -> [Ingredient] -> [Ingredient]
adjustIngredients factor = map adjustIngredient
  where adjustIngredient ingredient =
          ingredient{ quantity = quantity ingredient * factor }

showRecipe :: (String -> String) -> Recipe -> Maybe Int -> RichText
showRecipe t r maybeServings =  showRecipeHeader t r maybeServings
                ~~ "\n" ~~ unlines (showRecipeSteps r)

showRecipeHeader :: (String -> String) -> Recipe -> Maybe Int -> RichText
showRecipeHeader t r maybeServings = nameBox
                ~~ "\n" ~~ description r ~~ "\n"
                ~~ bold (t Str.headerServs) ~~ show servings ~~ "\n"
                ~~ bold (t Str.headerIngrs)
                ~~ unlines (map ((++) "* " . showIngredient servings) (ingredients r))
                where filler = replicate (length (recipeName r) + 2) '-'
                      servings = fromMaybe (servingSize r) maybeServings
                      nameBox = fontColor Magenta $ bold $ "+--" ~~ filler ~~ "+\n"
                                  ~~ "|  " ~~ recipeName r ~~ "  |\n"
                                  ~~ "+--" ~~ filler ~~ "+\n"

showRecipeSteps :: Recipe -> [String]
showRecipeSteps r =  zipWith (\i d -> "(" ++ show i ++ ") " ++ d) [1..] (directions r)

readRecipe :: [[String]] -> Recipe
readRecipe r = Recipe { recipeName = n, description = des, servingSize = s,
                        ingredients = i, directions = dir, tags = t }
  where n   = concat $ head r
        des = concat $ r !! 1
        s   = fromMaybe 1 $ readMaybe $ concat $ r !! 2
        i   = adjustIngredients (1 % s) $ readIngredients [r !! 3, r !! 4, r !! 5, r !! 6]
        dir = r !! 7
        unparsedTags = concat (r !! 8)
        t   = if ',' `elem` unparsedTags then
                map (dropWhile (== ' ')) $ splitOn "," unparsedTags
              else
                words unparsedTags

fillVoidTo :: [String] -> Int -> [String]
fillVoidTo xs n = if l < n then xs ++ replicate (n - l) "" else xs
  where l = length xs

fillVoid :: [[String]] -> Int -> [[String]]
fillVoid [] _ = []
fillVoid (x:xs) n = fillVoidTo x n : fillVoid xs n
