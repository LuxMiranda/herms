module UnitConversions where

import Types

-- NOTE: Here, "imperial" means "U.S. Customary". Conversion to British,
-- Australian, Canadian, etc. imperial units is not yet implemented.

data Conversion = Metric | Imperial | None deriving (Show, Read, Eq)

convertRecipeUnits :: Conversion -> Recipe -> Recipe
convertRecipeUnits unit recp =
  case unit of
    None        -> recp
    Metric      -> recp{ingredients = map convertIngredientToMetric (ingredients recp)}
    Imperial    -> recp{ingredients = map convertIngredientToImperial (ingredients recp)}

convertIngredientToMetric :: Ingredient -> Ingredient
convertIngredientToMetric ingr =
  case unit ingr of
    Tsp     -> ingr{quantity = quantity ingr * 5, unit = Ml}
    Tbsp    -> ingr{quantity = quantity ingr * 15, unit = Ml}
    Cup     -> ingr{quantity = quantity ingr * 30, unit = Ml}
    Oz      -> ingr{quantity = quantity ingr * 237, unit = Ml}
    FlOz    -> ingr{quantity = quantity ingr * 28, unit = G}
    Lb      -> ingr{quantity = quantity ingr * 454, unit = G}
    Pint    -> ingr{quantity = quantity ingr * 473, unit = Ml}
    Quart   -> ingr{quantity = quantity ingr * 946, unit = Ml}
    Gallon  -> ingr{quantity = quantity ingr * 3.785, unit = L}
    -- These cases are here so that if we add other units, the compiler will force us
    -- to add appropriate cases here.
    Ml      -> ingr
    L       -> ingr
    G       -> ingr
    Other _ -> ingr

convertIngredientToImperial :: Ingredient -> Ingredient
convertIngredientToImperial ingr =
  case unit ingr of
    Ml | quantity ingr < 15  -> ingr{quantity = quantity ingr / 5, unit = Tsp}
       | quantity ingr < 250 -> ingr{quantity = quantity ingr / 15, unit = Tbsp}
       | otherwise           -> ingr{quantity = quantity ingr / 250, unit = Cup}
    L  | quantity ingr < 4   -> ingr{quantity = quantity ingr * 4.23, unit = Cup}
       | otherwise           -> ingr{quantity = quantity ingr * 0.26, unit = Gallon}
    G                        -> ingr{quantity = quantity ingr / 28, unit = Oz}
    -- These cases are here so that if we add other units, the compiler will force us
    -- to add appropriate cases here.
    Tsp     -> ingr
    Tbsp    -> ingr
    Cup     -> ingr
    Oz      -> ingr
    FlOz    -> ingr
    Lb      -> ingr
    Pint    -> ingr
    Quart   -> ingr
    Gallon  -> ingr
    Other _ -> ingr
