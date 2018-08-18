module Main where
import Test.DocTest
main = doctest [ "-isrc"
               , "-iapp"
               -- , "src/Types.hs"
               ]
