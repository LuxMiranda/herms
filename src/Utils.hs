module Utils where

import Data.List (partition)
import Types (Ingredient (..), showFrac, showUnit)

safeLookup :: [a] -> Int -> Maybe a
safeLookup [] _ = Nothing
safeLookup (a : _) 0 = Just a
safeLookup (_ : as) n = safeLookup as (n - 1)

padLeft :: Int -> String -> String
padLeft n xs =
  let d = n - length xs
   in if d > 0
        then replicate d ' ' ++ xs
        else xs

padRight :: Int -> String -> String
padRight n xs =
  let d = n - length xs
   in if d > 0
        then xs ++ replicate d ' '
        else xs

combineIngredientsByFilter ::
  -- | The predicate to filter on
  (Ingredient -> Bool) ->
  -- | How to combine ingredients
  (Ingredient -> Ingredient -> Ingredient) ->
  -- | The list to combine
  [Ingredient] ->
  [Ingredient]
combineIngredientsByFilter pred comb lst =
  case partition pred lst of
    ([], rest) -> rest
    (x : xs, rest) -> foldl comb x xs : rest

-- | @combineIngredientsByName combines all the ingredients with the same
--   name and unit as the @key@.
combineIngredientsByName :: Ingredient -> [Ingredient] -> [Ingredient]
combineIngredientsByName key =
  combineIngredientsByFilter
    (\x -> ingredientName key == ingredientName x && unit key == unit x)
    addQuantities
  where
    addQuantities :: Ingredient -> Ingredient -> Ingredient
    addQuantities (Ingredient q1 u1 n1 a1) (Ingredient q2 u2 _ a2) =
      -- Show which attributes apply to what amount
      let attrs =
            if a1 /= a2
              then
                showFrac q1 ++ " " ++ showUnit u1 ++ ": " ++ a1
                  ++ ", "
                  ++ showFrac q2
                  ++ " "
                  ++ showUnit u2
                  ++ ": "
                  ++ a2
              else a1
       in Ingredient (q1 + q2) u1 n1 attrs

-- | @combineIngredients combines ingredients with identical names and units.
--   This is currently O(n^2), it could probably be better.
combineIngredients :: [Ingredient] -> [Ingredient]
combineIngredients lst = foldl (\acc x -> combineIngredientsByName x acc) lst lst
