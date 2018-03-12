module ReadConfig where

import UnitConversions
import Paths_herms

data Config = Config
  { defaultUnit        :: Conversion
  , defaultServingSize :: Int
  , recipesFile        :: String
  } deriving (Read, Show)

isNotComment :: String -> Bool
isNotComment []       = True
isNotComment (a:[])   = True
isNotComment (a:b:[]) = True
isNotComment (a:b:xs) = a /= '-' && b /= '-'

dropComments :: String -> String
dropComments = unlines . filter (isNotComment) . lines

getConfig :: IO Config
getConfig = do
  fileName <- getDataFileName "config.hs"
  contents <- readFile fileName
  return (read (dropComments contents) :: Config)

getDefaultUnit = do
  conf <- getConfig
  return (defaultUnit conf)
