{-# LANGUAGE OverloadedStrings #-}

module Html (toHtml, htmlDoc) where

import           Prelude hiding (head)

import           Types (showIngredient, Recipe(..))

tag :: String -> String -> String
tag t str = "<" ++ t ++ ">" ++ str ++ "</" ++ t ++ ">\n"

h1 :: String -> String
h1 = tag "h1"

h2 :: String -> String
h2 = tag "h2"

p :: String -> String
p = tag "p"

em :: String -> String
em = tag "em"

ol :: String -> String
ol = tag "ul"

ul :: String -> String
ul = tag "ul"

li :: String -> String
li = tag "li"

toHtml :: Recipe -> String
toHtml recipe =
  mconcat
    [ h1 $ recipeName recipe
    , p $ description recipe
    , p (em "Serving size:" ++ show (servingSize recipe))
    , h2 "Ingredients"
    , p $
        ul
        (concatMap
          (li . showIngredient 1)
          (ingredients recipe))
    , h2 $ "Directions"
    , ol (concatMap li (directions recipe))
    ]

html :: String -> String
html = tag "html"

head :: String -> String
head = tag "head"

title :: String -> String
title = tag "title"

body :: String -> String
body = tag "body"

htmlDoc :: Maybe String -> String -> String
htmlDoc cssUrl bodyHtml =
  mconcat
    [ "<!DOCTYPE html>"
    , html $
        mconcat
          [ head $
              mconcat
                [ case cssUrl of
                    Just url ->  "<link rel=\"stylesheet\" href=\"" ++ url ++ "\">"
                    Nothing -> ""
                , title "Recipes"
                ]

          , body bodyHtml
          ]
    ]
