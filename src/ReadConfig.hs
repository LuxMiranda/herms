module ReadConfig where

import UnitConversions
import qualified Text.Read as TR
import Control.Exception
import Data.Typeable
import Data.List.Split
import Lang.Pirate
import Paths_herms

------------------------------
------- Config Types ---------
------------------------------
  
-- TODO Allow record synonyms with that fancy stuff

data ConfigInfo = ConfigInfo
  { defaultUnit           :: Conversion
  , defaultServingSize    :: Int
  , recipesFile           :: String
  , language              :: String
  } deriving (Read, Show)

data Config = Config
  { defaultUnit'        :: Conversion
  , defaultServingSize' :: Int
  , recipesFile'        :: String
  , translator          :: String -> String
  } 

data Language = English
              | Pirate

type Translator = String -> String

------------------------------
---- Exception Handling ------
------------------------------

data ConfigParseError = ConfigParseError 
  deriving Typeable

instance Show ConfigParseError where
  show ConfigParseError = "Error parsing config.hs"

instance Exception ConfigParseError


------------------------------
--------- Functions ----------
------------------------------

getLang :: ConfigInfo -> Language
getLang c = case (language c) of
              "english" -> English
              "pirate" -> Pirate

getTranslator :: Language -> (String -> String)
getTranslator lang = case lang of
                       English -> id :: String -> String
                       Pirate  -> pirate

dropComments :: String -> String
dropComments = unlines . map (head . splitOn "--") . lines

processConfig :: ConfigInfo -> Config
processConfig raw = Config
  { defaultUnit'        = defaultUnit raw
  , defaultServingSize' = defaultServingSize raw
  , recipesFile'        = recipesFile raw
  , translator          = getTranslator $ getLang raw
  }

getConfig :: IO Config
getConfig = do
  fileName <- getDataFileName "config.hs"
  contents <- readFile fileName
  let result = TR.readEither (dropComments contents) :: Either String ConfigInfo
  case result of
    Left str -> throw ConfigParseError
    Right r  -> return (processConfig r)
