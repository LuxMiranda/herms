module UnitConversions.Tests where

import           Test.Tasty                (TestTree, testGroup)
import           Test.Tasty.QuickCheck     (testProperty)
import           Test.QuickCheck           ((==>), (===))

import           Types
import           Instances()
import           UnitConversions

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
