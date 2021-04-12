{-# LANGUAGE OverloadedStrings #-}

module Types.Tests where

import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (assertFailure, testCase, (@=?), (@?=))
import           Test.Tasty.QuickCheck   (NonNegative(..), property, testProperty, (===))

import           Data.List               (sort)
import           Data.Yaml               (decodeEither', encode)
import qualified Data.ByteString.Char8 as BS
import           Data.Ratio              ((%))
import           Instances()
import           Types

tests :: TestTree
tests = testGroup "Types" $ yaml :
    [ testCase "testParseUnitTbsp" $
        Tbsp @=? parseUnit "tbsp."
    , testCase "testParseUnitOther" $
        Other "foo" @=? parseUnit "foo"
    , testCase "testReadCup" $
        Cup @=? read "Cup"
    , testCase "testReadGals" $
        Gallon @=? read "Gallon"
    -- , testProperty "testReadIngredient" $
    --     (\i -> read (show (i :: Ingredient)) == i)
    , testCase "testParseIngredient" $
        Ingredient (1 % 1) Cup "" "" @=?
          read "Ingredient {quantity = 1 % 1, unit = Cup, ingredientName = \"\", attribute = \"\"}"
    , testCase "testShowIngredient" $
        "1/2 cup in, at" @=? showIngredient 1 (Ingredient (1 % 2) Cup "in" "at")
    , testCase "showFrac (proper)" $
        "4/7" @=? showFrac (4 % 7)
    , testCase "showFrac (improper)" $
        "1 3/13" @=? showFrac (16 % 13)
    , testCase "showFrac (irreducible)" $
        "2 1/4" @=? showFrac (81 % 36)
    , testCase "showFrac (integer)" $
        "9" @=? showFrac (81 % 9)
    , testCase "readFrac (proper)" $
        4 % 7 @=? readFrac "4/7"
    , testCase "readFrac (improper)" $
        16 % 13 @=? readFrac "1 3/13"
    , testCase "readFrac (irreducible)" $
        9 % 4 @=? readFrac "81/36"
    , testCase "readFrac (integer)" $
        9 % 1 @=? readFrac "9"
    , testProperty "readFrac . showFrac = id" $
        \y -> let x = getNonNegative y in x === readFrac (showFrac x)
    ]

yaml :: TestTree
yaml = testGroup "YAML"
  [ -- Decode inverts encode
    testProperty "testUnitToFromJson" $
     \i -> case decodeEither' (encode i) of
                Left _ -> property False
                Right decoded -> (i :: Unit) === decoded
  , testProperty "testIngredientToFromJson" $
     \i -> case decodeEither' (encode i) of
                Left _ -> property False
                Right decoded -> (i :: Ingredient) === decoded
  , testCase "testEncodeCup" $ BS.pack "cup\n" @=? encode Cup
  , testCase "testEncodeIngredient" $
    sort (BS.lines (encode (Ingredient (1 % 2) Quart "sugar" "brown"))) @?=
    ["attribute: brown", "name: sugar", "quantity: 1/2", "unit: quart"]
  , testCase "testEncodeIngredient (integer)" $
    sort (BS.lines (encode (Ingredient (2 % 2) Quart "sugar" "brown"))) @?=
    ["attribute: brown", "name: sugar", "quantity: 1", "unit: quart"]
  , testCase "testDecodeIngredient (integer)" $
    case decodeEither' "attribute: brown\nquantity: 1\nname: sugar\nunit: quart\n" of
         Left _ -> assertFailure "parse error"
         Right i -> i @?= Ingredient 1 Quart "sugar" "brown"
  ]
