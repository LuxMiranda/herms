{-# LANGUAGE OverloadedStrings #-}

module Find (findMatches, recipeToString) where

import Types
import RichText (toStr)
import Text.Regex.TDFA ((=~))

{-|
   `findMatches` takes a regex string, searching all fields of recipes for matching lines.
   The returned list of strings is in the form
   [Recipe name]: [matching line]
   -}
findMatches :: String -> [Recipe] -> [String]
findMatches rgx = concatMap zipName 
    where zipName r = 
            let name = recipeName r 
            in map (\b -> name ++ ": " ++ b) (findInRecipe rgx r)

findInRecipe :: String -> Recipe -> [String]
findInRecipe rgx = filter (\x -> x =~ rgx :: Bool) . lines . recipeToString

{-|
    `recipeToString` returns uncoloured output from RichText.toStr as a pure value. 
    This is the same text that would be rendered by calling `herms view`.
       -}
recipeToString :: Recipe -> String
recipeToString  = toStr False . flip (showRecipe id) (Just 1) 
          -- had to export toStr from RichText 
          -- False is to remove ANSI colour codes
          -- id is for the translator argument of showRecipe, 
            --  maybe add functionality for different language searches?
