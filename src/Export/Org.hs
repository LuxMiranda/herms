{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Export.Org (toOrg) where

import qualified Data.Char as Char
import qualified Data.List as List
import Data.String (IsString)
import Types (Recipe (..), showIngredient)
import Prelude hiding (succ)

newtype Org = Org {getOrg :: String}
  deriving (Eq, IsString, Monoid, Ord, Semigroup)

newtype Tag = Tag {getTag :: String}
  deriving (Eq, IsString, Monoid, Ord, Semigroup)

newtype HeaderLevel = HeaderLevel Int
  deriving (Eq, Ord)

succ :: HeaderLevel -> HeaderLevel
succ (HeaderLevel n) = HeaderLevel (n + 1)

one :: HeaderLevel
one = HeaderLevel 1

header :: HeaderLevel -> String -> Org
header (HeaderLevel n) s = Org $ take n (repeat '*') ++ (' ' : s)

ul :: [Org] -> Org
ul = unlinesOrg . map (Org . ("- " ++) . getOrg)

p :: String -> Org
p = Org

-- | Precondition: Input must be a header
tag :: [Tag] -> Org -> Org
tag tags =
  Org . (++ "  :" ++ List.intercalate ":" (map getTag tags) ++ ":") . getOrg

unlinesOrg :: [Org] -> Org
unlinesOrg = Org . unlines . map getOrg

recipeToOrg :: HeaderLevel -> Recipe -> Org
recipeToOrg level recipe =
  unlinesOrg
    [ tag (map Tag (tags recipe)) (header level (recipeName recipe)),
      p (description recipe),
      tag ["ingredients"] (header (succ level) "Ingredients"),
      ul
        ( map
            (Org . dropWhile Char.isSpace . showIngredient 1)
            (ingredients recipe)
        ),
      tag ["directions"] (header (succ level) "Directions"),
      ul (map Org (directions recipe))
    ]

toOrg :: [Recipe] -> String
toOrg recipes =
  getOrg $
    unlinesOrg
      [ header one "Recipes",
        unlinesOrg (map (recipeToOrg (succ one)) recipes)
      ]
