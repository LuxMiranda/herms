module Lang.Base where

import Lang.Pirate

data Language = English
              | Pirate

getLang :: Config -> Language
getLang c = case (language c) of
              "english" -> English
              "pirate" -> Pirate

toLang :: String -> Config -> String
toLang c s = case getLang c of
               English -> s
               Pirate  -> pirate s
