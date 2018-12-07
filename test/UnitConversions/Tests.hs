module UnitConversions.Tests where

import           Test.Tasty                (TestTree, testGroup)
import           Test.Tasty.QuickCheck     (testProperty)
import           Test.Tasty.HUnit          (testCase)
import           Test.QuickCheck           ((==>), (===))
import           Test.HUnit                ((@?=))

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
tests = testGroup "IngredientConversions"
    [ testProperty "testToMetric" $
        \x -> isMetric (unit x) ==>
                convertIngredientToMetric x === x
    , testProperty "testToImperial" $
        \x -> not (isMetric (unit x)) ==>
                convertIngredientToImperial x === x
    ]

temperatureUnitTests :: TestTree
temperatureUnitTests = testGroup "TemperatureConversions"
    [ testCase "Converting °F to °C in a text" $
        (convertTemperatureToMetric "preheat to 110°F") @?= "preheat to 43°C"
    , testCase "Converting °C to °F in a text" $
        (convertTemperatureToImperial "preheat to 43°C") @?= "preheat to 109°F"
        -- because of rounding we get 109°F when converting 110° F -> °C -> °F

    , testCase "Converting a negative temperature" $
        (convertTemperatureToMetric "-20°F") @?= "-29°C"
    , testCase "Converting a temperature in a text with a space between the number and the unit" $
        (convertTemperatureToImperial "50 °C") @?= "122°F"
    , testCase "Converting a temperature that doesn't have the degree sign" $
        (convertTemperatureToImperial "50 C") @?= "122°F"
    , testCase "Ignoring something that looks like a temperature but isn't" $
        (convertTemperatureToImperial "50 Crabs") @?= "50 Crabs"
    , testCase "Converting a temperature at the end of a sentence" $
        (convertTemperatureToImperial "50 C. 50C? 50 C!") @?= "122°F 122°F 122°F"
    ]
