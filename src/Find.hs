{-# LANGUAGE OverloadedStrings #-}

module Find (findMatches, recipeToString) where

import Types
import RichText (toStr)
import Text.Regex.TDFA ((=~))

findMatches :: String -> [Recipe] -> [String]
findMatches rgx = concatMap zipName 
    where zipName r = 
            let name = recipeName r 
             in zipWith (\a b -> a ++ ": " ++ b) (repeat name) . findInRecipe rgx $ r


findInRecipe :: String -> Recipe -> [String]
findInRecipe rgx = filter (\x -> x =~ rgx :: Bool) . lines . recipeToString

recipeToString :: Recipe -> String
recipeToString  = toStr False . flip (showRecipe id) (Just 1) 
          -- had to export toStr from RichText 
          -- False is to remove ANSI colour codes
          -- id is for the translator argument of showRecipe, 
            --  maybe add functionality for different language searches?

          

