module UnitConversions where

import Data.List
import Data.Ratio
import Data.Char (toLower)
import Types 

data Conversion = Metric | Imperial | None deriving (Show, Read, Eq)

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

convertIngredientToMetric :: Ingredient -> Ingredient
convertIngredientToMetric ingr =
    case getSynonym units of
        Just "tsp"   -> ingr{quantity = qty * 5, unit = "mL"}
        Just "Tbsp"  -> ingr{quantity = qty * 15, unit = "mL"}
        Just "cups"  -> ingr{quantity = qty * 250, unit = "mL"}
        Just "oz"    -> ingr{quantity = qty * 28, unit = "g"}
        Nothing      -> ingr
    where
        units = unit ingr
        qty = quantity ingr

convertIngredientToImperial :: Ingredient -> Ingredient
convertIngredientToImperial ingr =
    case un of
        "mL"   -> if qty < 15 then
                      ingr{quantity = qty / 5, unit = "tsp"}
                  else if qty < 250 then
                      ingr{quantity = qty / 15, unit = "Tbsp"}
                  else
                      ingr{quantity = qty / 250, unit = "cup(s)"}
        "g"    -> ingr{quantity = qty / 28, unit = "oz"}
        _      -> ingr
    where
        un = unit ingr
        qty = quantity ingr
