module Types.Tests where

import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (testCase, (@=?), (@?=))
import           Test.Tasty.QuickCheck   (testProperty)

import           Data.Yaml               (encode)
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
    ]

yaml :: TestTree
yaml = testGroup "YAML"
  [ -- Decode inverts encode
    -- TODO: Many examples succeed in the REPL, why do these fail?
  --   testProperty "testUnitToFromJson" $
  --   \u -> Right (u :: Unit)       == decodeEither' (encode u)
  -- , testProperty "testIngredientToFromJson" $
  --   \i -> Right (i :: Ingredient) == decodeEither' (encode i)

    testCase "testEncodeCup" $ BS.pack "cup\n" @=? encode Cup
  , testCase "testEncodeIngredient" $
    encode (Ingredient (1 % 2) Quart "sugar" "brown") @?=
    BS.pack "attribute: brown\nquantity: 1/2\nname: sugar\nunit: quart\n"
  ]
