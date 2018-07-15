module UnitConversions where

import Data.Char (toLower)
import Types

data Conversion = Metric | Imperial | None deriving (Show, Read, Eq)


--- NOTE: Here, "imperial" means "U.S. Customary". 
--- Conversion to British, Australian, Canadian, etc. imperial units is not yet implemented

convertRecipeUnits :: Conversion -> Recipe -> Recipe
convertRecipeUnits unit recp =
    case unit of
        None        -> recp
        Metric      -> recp{ingredients = map convertIngredientToMetric (ingredients recp)}
        Imperial    -> recp{ingredients = map convertIngredientToImperial (ingredients recp)}


getSynonym :: String -> Maybe String
getSynonym units
  | u `elem` tspSyns  = Just "tsp"
  | u `elem` tbspSyns = Just "Tbsp"
  | u `elem` cupSyns  = Just "cups"
  | u `elem` ozSyns   = Just "oz"
  | u `elem` flOzSyns = Just "fl oz"
  | u `elem` lbsSyns  = Just "lbs"
  | u `elem` pintSyns = Just "pint"
  | u `elem` quartSyns= Just "quart"
  | u `elem` galSyns  = Just "gallon"
  | otherwise = Nothing
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


convertIngredientToMetric :: Ingredient -> Ingredient
convertIngredientToMetric ingr =
    case getSynonym units of
        Just "tsp"   -> ingr{quantity = qty * 5, unit = "mL"}
        Just "Tbsp"  -> ingr{quantity = qty * 15, unit = "mL"}
        Just "fl oz" -> ingr{quantity = qty * 30, unit = "mL"}
        Just "cups"  -> ingr{quantity = qty * 237, unit = "mL"}
        Just "oz"    -> ingr{quantity = qty * 28, unit = "g"}
        Just "lbs"   -> ingr{quantity = qty * 454, unit = "g"}
        Just "pint"  -> ingr{quantity = qty * 473, unit = "mL"}
        Just "quart" -> ingr{quantity = qty * 946, unit = "mL"}
        Just "gallon"-> ingr{quantity = qty * 3.785, unit = "L"}
        _            -> ingr
    where
        units = unit ingr
        qty = quantity ingr

convertIngredientToImperial :: Ingredient -> Ingredient
convertIngredientToImperial ingr =
    case un of
        "mL" | qty < 15  -> ingr{quantity = qty / 5, unit = "tsp"}
             | qty < 250 -> ingr{quantity = qty / 15, unit = "Tbsp"}
             | otherwise ->  ingr{quantity = qty / 250, unit = "cup(s)"}

        "g"    -> ingr{quantity = qty / 28, unit = "oz"}
        _      -> ingr
    where
        un = unit ingr
        qty = quantity ingr
