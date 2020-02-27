module Instances where

import           Test.QuickCheck (NonNegative(..), oneof)
import           Test.QuickCheck.Arbitrary (Arbitrary(..), genericShrink)

import           Types

instance Arbitrary Unit where
  arbitrary = oneof
    [ pure Tsp
    , pure Tbsp
    , pure Cup
    , pure Oz
    , pure FlOz
    , pure Lb
    , pure Pint
    , pure Quart
    , pure Gallon
    , pure Ml
    , pure L
    , pure G
    -- We need to ensure that units constructed via Other should not be
    -- a synonym for other units. This is definitely not ideal as changes
    -- to parseUnit will change the distribution of `Arbitrary Unit`. This
    -- is a hacky fix for now to avoid flaky tests.
    , parseUnit <$> arbitrary
    ]

instance Arbitrary Ingredient where
  arbitrary = Ingredient
    <$> (getNonNegative <$> arbitrary)
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink = genericShrink
