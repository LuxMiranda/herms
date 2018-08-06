module Main (main) where

import qualified Test.Tasty as T
import qualified ReadConfig.Tests
import qualified UnitConversions.Tests

main :: IO ()
main = T.defaultMain $ T.testGroup "Tests" [ ReadConfig.Tests.tests
                                           , UnitConversions.Tests.tests
                                           ]
