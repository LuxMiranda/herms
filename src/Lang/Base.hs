module Lang.Base where

{--
data Language = English
              | Pirate

getLang :: RawConfig -> Language
getLang c = case (language c) of
              "english" -> English
              "pirate" -> Pirate

getTranslator :: Language -> (String -> String)
getTranslator lang = case lang of
                       English -> id :: String -> String
                       Pirate  -> pirate
                       --}
