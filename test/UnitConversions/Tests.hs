module UnitConversions.Tests where

import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (testCase)
import           Test.Tasty.QuickCheck   (testProperty)
import           Test.HUnit              ((@?=))

import           UnitConversions

tests :: TestTree
tests = testGroup "UnitConversions"
    [ testCase "testGetSynonymTbsp" $
        getSynonym "tbsp" @?= Just "Tbsp"
    ]
