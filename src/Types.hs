{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Brick.Widgets.Core (TextWidth (..))
import Control.Applicative
import Data.Char (digitToInt, isDigit, toLower)
import Data.List
import qualified Data.List as List
import qualified Data.List.Split as List
import Data.Maybe
import Data.Monoid ()
import Data.Ord
import Data.Ratio
import qualified Data.Text as Text hiding (toLower)
import Data.Yaml ((.:), (.:?), (.=))
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import qualified Lang.Strings as Str
import RichText
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (Read (..), readMaybe)

data Unit
  = -- Imperial
    Tsp
  | Tbsp
  | Cup
  | Oz
  | FlOz
  | Lb
  | Pint
  | Quart
  | Gallon
  | -- Metric
    Ml -- deceptively, this is milliliters (mL)
  | L -- liters (L)
  | G -- grams (g)
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
    Tsp -> "tsp"
    Tbsp -> "Tbsp"
    Cup -> "cup"
    Oz -> "oz"
    FlOz -> "fl oz"
    Lb -> "lb"
    Pint -> "pint"
    Quart -> "quart"
    Gallon -> "gallon"
    -- Metric
    Ml -> "mL"
    L -> "L"
    G -> "g"
    Other str -> str

instance Yaml.ToJSON Unit where
  toJSON = Yaml.String . Text.pack . showUnit

parseUnit :: String -> Unit
parseUnit units
  | u `elem` tspSyns = Tsp
  | u `elem` tbspSyns = Tbsp
  | u `elem` cupSyns = Cup
  | u `elem` ozSyns = Oz
  | u `elem` flOzSyns = FlOz
  | u `elem` lbsSyns = Lb
  | u `elem` pintSyns = Pint
  | u `elem` quartSyns = Quart
  | u `elem` galSyns = Gallon
  | u `elem` gSyns = G
  | u `elem` mlSyns = Ml
  | u `elem` lSyns = L
  | otherwise = Other units
  where
    u = List.map toLower units
    gSyns =
      [ "g",
        "grams"
      ]

    lSyns =
      [ "l",
        "L",
        "liter",
        "litre",
        "liters",
        "litres"
      ]

    mlSyns =
      [ "ml",
        "mL",
        "milliliter",
        "millilitre",
        "milliliters",
        "millilitres"
      ]

    tspSyns =
      [ "tsp",
        "tsp.",
        "teaspoon",
        "teaspoons"
      ]

    tbspSyns =
      [ "tbsp",
        "tbsp.",
        "tablespoon",
        "tablespoons"
      ]

    cupSyns =
      [ "cup",
        "cups",
        "cp",
        "cps",
        "cp.",
        "cps."
      ]

    ozSyns =
      [ "oz",
        "oz.",
        "ounce",
        "ounces"
      ]

    flOzSyns =
      [ "fl oz",
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
        "fl. ounces"
      ]

    lbsSyns =
      [ "lb.",
        "lbs.",
        "lb",
        "lbs",
        "pound",
        "pounds"
      ]

    pintSyns =
      [ "pt",
        "pt.",
        "pts",
        "pts.",
        "pint",
        "pints"
      ]

    quartSyns =
      [ "qt",
        "qt.",
        "qts",
        "qts.",
        "quart",
        "quarts"
      ]

    galSyns =
      [ "gal",
        "gal.",
        "gals",
        "gals.",
        "gallon",
        "gallons"
      ]

instance Yaml.FromJSON Unit where
  parseJSON = Yaml.withText "Unit" $ pure . parseUnit . Text.unpack

data Ingredient = Ingredient
  { quantity :: Ratio Int,
    unit :: Unit,
    ingredientName :: String,
    attribute :: String
  }
  deriving (Generic, Eq, Show, Read)

instance Yaml.ToJSON Ingredient where
  toJSON u =
    Yaml.object
      [ "quantity" .= fracToValue (quantity u),
        "unit" .= Text.pack (showUnit $ unit u),
        "name" .= Text.pack (ingredientName u),
        "attribute" .= Text.pack (attribute u)
      ]

instance Yaml.FromJSON Ingredient where
  parseJSON = Yaml.withObject "Ingredient" $ \o ->
    Ingredient
      <$> ((readFrac <$> o .: "quantity") <|> (fromIntegral <$> (o .: "quantity" :: Yaml.Parser Int)))
      <*> (parseUnit <$> withDefault o "" "unit")
      <*> (o .: "name")
      <*> withDefault o "" "attribute"
    where
      withDefault o def name =
        fromMaybe def <$> o .:? Text.pack name

instance Ord Ingredient where
  ingr1 `compare` ingr2 =
    case ingredientName ingr1 `compare` ingredientName ingr2 of
      EQ -> case unit ingr1 `compare` unit ingr2 of
        EQ -> case attribute ingr1 `compare` attribute ingr2 of
          EQ -> quantity ingr1 `compare` quantity ingr2
          x -> x
        x -> x
      x -> x

data Recipe = Recipe
  { recipeName :: String,
    description :: String,
    servingSize :: Int,
    ingredients :: [Ingredient],
    directions :: [String],
    tags :: [String]
  }
  deriving (Eq, Generic, Show, Read)

-- instance Yaml.ToJSON [Ingredient] where

instance Yaml.ToJSON Recipe where
  toJSON r =
    Yaml.object
      [ "name" .= Text.pack (recipeName r),
        "description" .= Text.pack (description r),
        "serving size" .= servingSize r,
        "ingredients" .= Yaml.toJSON (ingredients r),
        "directions" .= map Text.pack (directions r),
        "tags" .= map Text.pack (tags r)
      ]

instance Yaml.FromJSON Recipe where
  parseJSON = Yaml.withObject "Recipe" $ \o ->
    Recipe
      <$> (o .: "name")
      <*> withDefault o "" "description"
      <*> withDefault o 1 "serving size"
      <*> (o .: "ingredients")
      <*> (o .: "directions")
      <*> withDefault o [] "tags"
    where
      withDefault o def name =
        fromMaybe def <$> o .:? Text.pack name

type RecipeBook = [Recipe]

-- | @showFrac displays improper fractions
--
-- >>> showFrac (2 / 3)
-- "2/3"
--
-- >>> showFrac (5 / 4)
-- "1 1/4"
showFrac :: Ratio Int -> String
showFrac x
  | r == 0 = show q
  | q > 0 = show q <> " " <> showFrac (r % d)
  | otherwise = show n <> "/" <> show d
  where
    (n, d) = (numerator x, denominator x)
    (q, r) = n `quotRem` d

fracToValue :: Ratio Int -> Yaml.Value
fracToValue x
  | r == 0 = Yaml.Number $ fromIntegral q
  | otherwise = Yaml.String . Text.pack $ showFrac x
  where
    (n, d) = (numerator x, denominator x)
    (q, r) = n `quotRem` d

newtype Fraction = Fraction {getFraction :: Ratio Int}
  deriving (Eq, Ord, Show)

instance Read Fraction where
  readPrec = ReadPrec.lift $ Fraction <$> parser
    where
      int = foldl' (\x -> ((x * 10) +) . digitToInt) 0 <$> ReadP.munch isDigit <* ReadP.skipSpaces
      slash = ReadP.char '/' *> ReadP.skipSpaces
      frac = (%) <$> int <*> (slash *> int)
      parser = do
        x <- int
        r <- ReadP.look
        case r of
          [] -> pure (fromIntegral x)
          ('/' : _) -> (x %) <$> (slash *> int)
          _ -> (fromIntegral x +) <$> frac

readFrac :: String -> Ratio Int
readFrac = getFraction . read

-- | @showIngredient is for user-friendly printing
--
-- >>> showIngredient (Ingredient (1 % 2) Cup "ingr" "attr")
-- "1/2 Cup ingr, attr"
showIngredient :: Int -> Ingredient -> String
showIngredient servings i =
  qty ++ showUnit (unit i) ++ " " ++ ingredientName i ++ att
  where
    qty =
      if quantity i == 0
        then ""
        else showFrac (quantity i * (servings % 1)) ++ " "
    att = if List.null (attribute i) then "" else ", " ++ attribute i

makeIngredients :: Functor f => f [String] -> f Ingredient
makeIngredients = fmap makeIngredients'
  where
    makeIngredients' (q : u : i : a : _) =
      Ingredient
        { quantity = if null q then 0 else readFrac q,
          unit = parseUnit u,
          ingredientName = i,
          attribute = a
        }
    makeIngredients' _ = error "length of input to makeIngredients' must be > 3"

readIngredients :: [[String]] -> [Ingredient]
readIngredients is =
  makeIngredients $
    List.transpose $
      fillVoid is $
        List.length $
          List.maximumBy (comparing List.length) is

adjustIngredients :: Ratio Int -> [Ingredient] -> [Ingredient]
adjustIngredients factor =
  List.map (\ingr -> ingr {quantity = quantity ingr * factor})

newline :: RichText
newline = "\n"

showRecipe :: (String -> String) -> Recipe -> Maybe Int -> RichText
showRecipe t r maybeServings =
  showRecipeHeader t r serv
    ~~ newline
    ~~ List.unlines (showRecipeSteps r)
  where
    serv = case maybeServings of
      --                   Nothing       -> "Error: use a serving size greater than 0\n"
      Nothing -> Just 1 -- default to 1 if no servings specified
      _ -> maybeServings

showRecipeHeader :: (String -> String) -> Recipe -> Maybe Int -> RichText
showRecipeHeader t r maybeServings =
  nameBox
    ~~ newline
    ~~ description r
    ~~ newline
    ~~ bold (t Str.headerServs)
    ~~ show servings
    ~~ newline
    ~~ bold (t Str.headerIngrs)
    ~~ List.unlines (List.map ((++) "* " . showIngredient servings) (ingredients r))
  where
    filler = List.replicate (textWidth (recipeName r) + 2) '-'
    servings = fromMaybe (servingSize r) maybeServings
    horizontalLine = ("+--" :: RichText) ~~ filler ~~ ("+" :: RichText) ~~ newline
    nameBox =
      fontColor Magenta $
        bold $
          horizontalLine
            ~~ ("|  " :: RichText)
            ~~ recipeName r
            ~~ ("  |" :: RichText)
            ~~ newline
            ~~ horizontalLine

showRecipeSteps :: Recipe -> [String]
showRecipeSteps r =
  List.zipWith (\i d -> "(" ++ show i ++ ") " ++ d) [1 ..] (directions r)

readRecipe :: [[String]] -> Recipe
readRecipe r =
  Recipe
    { recipeName = n,
      description = des,
      servingSize = s,
      ingredients = i,
      directions = dir,
      tags = t
    }
  where
    n = List.concat $ List.head r
    des = List.concat $ r !! 1
    s = fromMaybe 1 $ readMaybe $ List.concat $ r !! 2
    i = adjustIngredients (1 % s) $ readIngredients [r !! 3, r !! 4, r !! 5, r !! 6]
    dir = r !! 7
    unparsedTags = List.concat (r !! 8)
    t =
      if ',' `elem` unparsedTags
        then List.map (List.dropWhile (== ' ')) $ List.splitOn "," unparsedTags
        else List.words unparsedTags

fillVoidTo :: [String] -> Int -> [String]
fillVoidTo xs n =
  if l < n
    then xs ++ List.replicate (n - l) ""
    else xs
  where
    l = List.length xs

fillVoid :: [[String]] -> Int -> [[String]]
fillVoid [] _ = []
fillVoid (x : xs) n = fillVoidTo x n : fillVoid xs n
