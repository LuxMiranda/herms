module Types.Tests where

import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (testCase, (@=?))
-- import           Test.Tasty.QuickCheck   (testProperty)

import           Data.Ratio              ((%))
import           Instances()
import           Types

tests :: TestTree
tests = testGroup "Types"
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
    ]
