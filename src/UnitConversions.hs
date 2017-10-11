module UnitConversions where

import Data.List
import Data.Ratio

--unitType: 0 = Metric | 1 = Imperial
convertRecipeUnits :: Int -> Recipe -> Recipe
convertRecipeUnits un recp =
    if un == 0 then
        recp{ingredients = map convertIngredientToMetric (ingredients recp)}
    else
        recp{ingredients = map convertIngredientToImperial (ingredients recp)}

convertIngredientToMetric :: Ingredient -> Ingredient
convertIngredientToMetric ingr =
    case un of
        "tsp"   -> ingr{quantity = qty * 5, unit = "mL"}
        "Tbsp"  -> ingr{quantity = qty * 15, unit = "mL"}
        "cup"   -> ingr{quantity = qty * 250, unit = "mL"}
        "oz"    -> ingr{quantity = qty * 28, unit = "g"}
        _       -> ingr
    where
        un = unit ingr
        qty = quantity ingr

convertIngredientToImperial :: Ingredient -> Ingredient
convertIngredientToImperial ingr =
    case un of
        "mL"   -> if qty < 15 then
                      ingr{quantity = qty / 5, unit = "tsp"}
                  else if qty < 250 then
                      ingr{quantity = qty / 15, unit = "Tbsp"}
                  else
                      ingr{quantity = qty / 250, unit = "cup"}
        "g"    -> ingr{quantity = qty / 28, unit = "oz"}
        _      -> ingr
    where
        un = unit ingr
        qty = quantity ingr