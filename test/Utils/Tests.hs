module Utils.Tests where

import Data.Ratio ((%))
import Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Test.Tasty.QuickCheck (testProperty, (==>))
import Types
import Utils

tests :: TestTree
tests =
  testGroup
    "Utils"
    [ testCase "combineIngredients" $
        let i q = Ingredient q Cup "oil" "attr"
         in [i (1 % 1)] @=? combineIngredients [i (1 % 2), i (1 % 2)],
      testCase "combineIngredients" $
        let i q = Ingredient q Lb "flour" "wheat"
         in [i (3 % 4)] @=? combineIngredients [i (1 % 2), i (1 % 4)],
      testProperty "combineIngredientsLess" $
        \l -> length (combineIngredients l) <= length l,
      testProperty "combineIngredientsSingleton" $
        \i -> [i] == combineIngredients [i],
      testProperty "combineIngredientsUniform" $
        \i n -> (n >= 1) ==> (1 == length (combineIngredients (replicate n i)))
    ]
