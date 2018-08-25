module Main (main) where

import qualified Test.Tasty as T

import qualified ReadConfig.Tests
import qualified Types.Tests
import qualified UnitConversions.Tests
import qualified Utils.Tests

main :: IO ()
main = T.defaultMain $ T.testGroup "Tests" [ ReadConfig.Tests.tests
                                           , Types.Tests.tests
                                           , Utils.Tests.tests
                                           , UnitConversions.Tests.tests
                                           ]
