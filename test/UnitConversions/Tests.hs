module UnitConversions.Tests where

import           Test.Tasty                (TestTree, testGroup)
import           Test.Tasty.QuickCheck     (testProperty)
import           Test.Tasty.HUnit          (testCase)
import           Test.QuickCheck           ((==>), (===))
import           Test.HUnit                ((@?=), (@=?))
import           Data.Tuple                (swap)

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
        -- because of rounding we get 109°F when converting 110° F -> °C -> °F
        (convertTemperatureToImperial "preheat to 43°C") @?= "preheat to 109°F"
        
    , testCase "Recognizing temperatures in a text" $
        let getTemperatures = map snd . findReplacements
            testCases = [ ("250°C", [Temperature 250 C])
                        , ("10 °C", [Temperature 10 C])
                        , ("-10 °C", [Temperature (-10) C])
                        , ("abc 15 C def", [Temperature 15 C])
                        , ("5 C", [Temperature 5 C])
                        , ("250C", [Temperature 250 C])
                        , ("999°F", [Temperature 999 F])
                        , ("34F", [Temperature 34 F])
                        , ("250°C.", [Temperature 250 C])
                        , ("250°C!", [Temperature 250 C])
                        , ("250°C?", [Temperature 250 C])
                        , ("10C 15F", [Temperature 10 C, Temperature 15 F])
                        , ("0 Frites", [])
                        , ("250Crabs", [])
                        , ("250 Crabs", [])] :: [(String, [Temperature])] in
            sequence_ $ map (uncurry (@=?) . fmap getTemperatures . swap) testCases
    ]
