module ReadConfig where

import UnitConversions
import qualified Text.Read as TR
import Control.Exception
import Data.Typeable
import Data.Char (toLower)
import Data.List.Split
import Lang.Pirate
import Lang.French
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
              | Portuguese
              | French

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
------ Language Synonyms -----
------------------------------

englishSyns = [ "english"
              , "en"
              , "en-us"
              , "\'murican"
              ]

pirateSyns = [ "pirate"
             , "pr"
             ]

portugueseSyns = [ "portuguese"
                 , "português"
                 , "portugues"
                 , "pt"
                 ]

frenchSyns = [ "french"
              , "fr"
              , "fr-fr"
              , "français"
              , "francais"
              ]


------------------------------
--------- Functions ----------
------------------------------

getLang :: ConfigInfo -> Language
getLang c
  | isIn englishSyns = English
  -- | isIn portugueseSyns = Portuguese
  | isIn pirateSyns  = Pirate
  -- | isIn frenchSyns = French
  | isIn frenchSyns  = French
  where isIn = elem (map toLower $ language c)

getTranslator :: Language -> Translator
getTranslator lang = case lang of
                       English -> id :: String -> String
                       Pirate  -> pirate
                       French  -> french

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
