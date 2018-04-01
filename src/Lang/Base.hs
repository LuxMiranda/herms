module Lang.Base where

import ReadConfig
import Lang.Pirate

data Language = English
              | Pirate

type Translator = String -> String

getLang :: Config -> Language
getLang c = case (language c) of
              "english" -> English
              "pirate" -> Pirate

toLang :: Config -> String -> String
toLang c s = case getLang c of
               English -> s
               Pirate  -> pirate s
