module Herms.Utils where

safeLookup :: [a] -> Int -> Maybe a
safeLookup []       _ = Nothing
safeLookup (a : _)  0 = Just a
safeLookup (_ : as) n = safeLookup as (n - 1)


padLeft :: Int -> String -> String
padLeft n xs =
  let d = n - length xs in
  if d > 0 then replicate d ' ' ++ xs
  else xs

padRight :: Int -> String -> String
padRight n xs =
  let d = n - length xs in
  if d > 0 then xs ++ replicate d ' '
  else xs

