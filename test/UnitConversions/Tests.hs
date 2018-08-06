module UnitConversions.Tests where

import           Test.Tasty                (TestTree, testGroup)
-- import           Test.Tasty.HUnit          (testCase)
import           Test.Tasty.QuickCheck     (testProperty)
-- import           Test.HUnit                ((@?=))
import           Test.QuickCheck           ((==>), (===))

-- New Arbitrary instances
import           Generic.Random
import           Test.QuickCheck.Arbitrary (Arbitrary(..))

import           Types
import           UnitConversions

instance Arbitrary Unit where
  arbitrary = genericArbitrary uniform

instance Arbitrary Ingredient where
  arbitrary = genericArbitrary uniform

isMetric :: Unit -> Bool
isMetric unit =
  case unit of
    Ml -> True
    L  -> True
    G  -> True
    _  -> False

tests :: TestTree
tests = testGroup "UnitConversions"
    [ testProperty "testToMetric" $
        \x -> isMetric (unit x) ==>
                convertIngredientToMetric x === x
    , testProperty "testToImperial" $
        \x -> not (isMetric (unit x)) ==>
                convertIngredientToImperial x === x
    ]
