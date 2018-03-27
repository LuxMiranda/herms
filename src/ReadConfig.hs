module ReadConfig where

import UnitConversions
import qualified Text.Read as TR
import Control.Exception
import Data.Typeable
import Data.List.Split
import Paths_herms

data Config = Config
  { defaultUnit        :: Conversion
  , defaultServingSize :: Int
  , recipesFile        :: String
  } deriving (Read, Show)

data ConfigParseError = ConfigParseError 
  deriving Typeable

instance Show ConfigParseError where
  show ConfigParseError = "Error parsing config.hs"

instance Exception ConfigParseError

dropComments :: String -> String
dropComments = unlines . map (head . splitOn "--") . lines

getConfig :: IO Config
getConfig = do
  fileName <- getDataFileName "config.hs"
  contents <- readFile fileName
  let result = TR.readEither (dropComments contents) :: Either String Config
  case result of
    Left str -> throw ConfigParseError
    Right r  -> return r
