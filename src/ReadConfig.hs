module ReadConfig where

import UnitConversions

data Config = Config
  { defaultUnit        :: Conversion
  , defaultServingSize :: Int
  , recipesFile        :: String
  } deriving Read
{--
getConfig :: IO Config
getConfig = do
  fileName <- getDataFileName "config.hs"
  contents <- readFile fileName
  return (read contents :: Config)
  --}
