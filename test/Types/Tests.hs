module Types.Tests where

import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (testCase)
-- import           Test.Tasty.QuickCheck   (testProperty)
import           Test.HUnit              ((@?=))

-- New Arbitrary instances
import           Generic.Random hiding ((%))
import           Test.QuickCheck.Arbitrary (Arbitrary(..))

import           Data.Ratio              ((%))
import           Types

instance Arbitrary Unit where
  arbitrary = genericArbitrary uniform

instance Arbitrary Ingredient where
  arbitrary = genericArbitrary uniform

tests :: TestTree
tests = testGroup "Types"
    [ testCase "testParseUnitTbsp" $
        parseUnit "tbsp." @?= Tbsp
    , testCase "testParseUnitOther" $
        parseUnit "foo" @?= Other "foo"
    , testCase "testReadCup" $
        read "Cup" @?= Cup
    , testCase "testReadGals" $
        read "Gallon" @?= Gallon
    -- , testProperty "testReadIngredient" $
    --     (\i -> read (show (i :: Ingredient)) == i)
    , testCase "testParseIngredient" $
        read "Ingredient {quantity = 1 % 1, unit = Cup, ingredientName = \"\", attribute = \"\"}" @?=
        Ingredient { quantity = (1 % 1)
                   , unit = Cup
                   , ingredientName = ""
                   , attribute = ""
                   }
    ]
