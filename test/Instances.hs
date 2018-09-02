module Instances where

import           Generic.Random hiding ((%))
import           Test.QuickCheck.Arbitrary (Arbitrary(..))

import           Types

-- New Arbitrary instances

instance Arbitrary Unit where
  arbitrary = genericArbitrary uniform

instance Arbitrary Ingredient where
  arbitrary = genericArbitrary uniform
