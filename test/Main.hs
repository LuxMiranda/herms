module Main (main) where

import qualified Test.Tasty as T
import qualified UnitConversions.Tests

main :: IO ()
main = T.defaultMain $ T.testGroup "Tests" [ UnitConversions.Tests.tests
                                           ]
