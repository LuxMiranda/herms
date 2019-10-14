{-# LANGUAGE DeriveGeneric #-}

module Types where

import           Data.Char    (toLower)
import           Data.Maybe
import           Data.Ord
import           Data.Ratio
import           Data.Yaml    ((.=), (.:))
import           GHC.Generics (Generic)
import           Text.Read    (readMaybe)
import qualified Data.List       as List
import qualified Data.List.Split as List
import qualified Data.Text       as Text hiding (toLower)
import qualified Data.Yaml       as Yaml

import qualified Lang.Strings    as Str
import           RichText

data Format =
    JSON
  | YAML

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

instance Yaml.ToJSON Unit where
  toJSON = Yaml.String . Text.pack . showUnit

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
  where u = List.map toLower units
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

instance Yaml.FromJSON Unit where
  parseJSON = Yaml.withText "Unit" $ pure . parseUnit . Text.unpack

data Ingredient = Ingredient { quantity       :: Ratio Int
                             , unit           :: Unit
                             , ingredientName :: String
                             , attribute      :: String
                             } deriving (Generic, Eq, Show, Read)

instance Yaml.ToJSON Ingredient where
  toJSON u = Yaml.object [ Text.pack "quantity"  .= (Text.pack $ showFrac $ quantity u)
                         , Text.pack "unit"      .= (Text.pack $ showUnit $ unit u)
                         , Text.pack "name"      .= (Text.pack $ ingredientName u)
                         , Text.pack "attribute" .= (Text.pack $ attribute u)
                         ]

instance Yaml.FromJSON Ingredient where
  parseJSON = Yaml.withObject "Ingredient" $ \o -> Ingredient
    <$> (readFrac  <$> o .: Text.pack "quantity")
    <*> (parseUnit <$> o .: Text.pack "unit")
    <*> (o .: Text.pack "name")
    <*> (o .: Text.pack "attribute")

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

-- instance Yaml.ToJSON [Ingredient] where

instance Yaml.ToJSON Recipe where
  toJSON r = Yaml.object $
    [ Text.pack "name"         .= (Text.pack $ recipeName r)
    , Text.pack "description"  .= (Text.pack $ description r)
    , Text.pack "serving size" .= servingSize r
    , Text.pack "ingredients"  .= (Yaml.toJSON $ ingredients r)
    , Text.pack "directions"   .= (map Text.pack $ directions r)
    , Text.pack "tags"         .= (map Text.pack $ tags r)
    ]

instance Yaml.FromJSON Recipe where
  parseJSON = Yaml.withObject "Recipe" $ \o -> Recipe
    <$> (o .: Text.pack "name")
    <*> (o .: Text.pack "description")
    <*> (o .: Text.pack "serving size")
    <*> (o .: Text.pack "ingredients")
    <*> (o .: Text.pack "directions")
    <*> (o .: Text.pack "tags")

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
  | ' ' `elem` x = let xs = List.splitOn " " x
                   in (read' (List.head xs) % 1) + readFrac (List.last xs)
  | '/' `elem` x = let xs = List.splitOn "/" x
                   in read' (List.head xs) % read' (List.last xs) | otherwise = (read x :: Int) % 1
  where read' = read :: String -> Int

-- | @showIngredient is for user-friendly printing
--
-- >>> showIngredient (Ingredient (1 % 2) Cup "ingr" "attr")
-- "1/2 Cup ingr, attr"
--
showIngredient :: Int -> Ingredient -> String
showIngredient servings i =
  qty ++ showUnit (unit i) ++ " " ++ ingredientName i ++ att
  where qty = if quantity i == 0
              then ""
              else showFrac (quantity i * (servings % 1)) ++ " "
        att = if List.null (attribute i) then "" else ", " ++ attribute i

-- TODO: rewrite as a `fold`
makeIngredients :: [[String]] -> [Ingredient]
makeIngredients [] = []
makeIngredients (i:is) =
  Ingredient { quantity       = case i of
                                  (xs@(_ : _) : _) -> readFrac xs
                                  _                -> 0 % 1
             , unit           = parseUnit (i !! 1)
             , ingredientName = i !! 2
             , attribute      = i !! 3
             } : makeIngredients is


readIngredients :: [[String]] -> [Ingredient]
readIngredients is =
  makeIngredients $ List.transpose $ fillVoid is $ List.length $
    List.maximumBy (comparing List.length) is

adjustIngredients :: Ratio Int -> [Ingredient] -> [Ingredient]
adjustIngredients factor =
  List.map (\ingr -> ingr { quantity = quantity ingr * factor })

showRecipe :: (String -> String) -> Recipe -> Maybe Int -> RichText
showRecipe t r maybeServings =
  if maybeServings == Nothing
  then "Error: use a serving size greater than 0" ~~ "\n"
  else showRecipeHeader t r maybeServings
       ~~ "\n" ~~ List.unlines (showRecipeSteps r)

showRecipeHeader :: (String -> String) -> Recipe -> Maybe Int -> RichText
showRecipeHeader t r maybeServings = nameBox
                ~~ "\n" ~~ description r ~~ "\n"
                ~~ bold (t Str.headerServs) ~~ show servings ~~ "\n"
                ~~ bold (t Str.headerIngrs)
                ~~ List.unlines (List.map ((++) "* " . showIngredient servings) (ingredients r))
                where filler = List.replicate (List.length (recipeName r) + 2) '-'
                      servings = fromMaybe (servingSize r) maybeServings
                      nameBox = fontColor Magenta $ bold $ "+--" ~~ filler ~~ "+\n"
                                  ~~ "|  " ~~ recipeName r ~~ "  |\n"
                                  ~~ "+--" ~~ filler ~~ "+\n"

showRecipeSteps :: Recipe -> [String]
showRecipeSteps r =
  List.zipWith (\i d -> "(" ++ show i ++ ") " ++ d) [1..] (directions r)

readRecipe :: [[String]] -> Recipe
readRecipe r = Recipe { recipeName = n, description = des, servingSize = s,
                        ingredients = i, directions = dir, tags = t }
  where n   = List.concat $ List.head r
        des = List.concat $ r !! 1
        s   = fromMaybe 1 $ readMaybe $ List.concat $ r !! 2
        i   = adjustIngredients (1 % s) $ readIngredients [r !! 3, r !! 4, r !! 5, r !! 6]
        dir = r !! 7
        unparsedTags = List.concat (r !! 8)
        t   =
          if ',' `elem` unparsedTags
          then List.map (List.dropWhile (== ' ')) $ List.splitOn "," unparsedTags
          else List.words unparsedTags

fillVoidTo :: [String] -> Int -> [String]
fillVoidTo xs n =
  if l < n
  then xs ++ List.replicate (n - l) ""
  else xs
  where l = List.length xs

fillVoid :: [[String]] -> Int -> [[String]]
fillVoid [] _ = []
fillVoid (x:xs) n = fillVoidTo x n : fillVoid xs n
