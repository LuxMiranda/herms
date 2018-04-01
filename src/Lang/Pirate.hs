module Lang.Pirate where

import Lang.Strings

pirate :: String -> String
pirate str
  | is listDesc     = "list yer recipes"
  | is viewDesc     = "gander at yer particular recipes"
  | is removeDesc   = "show a recipe to the plank"
  | is shoppingDesc = "make a list o' needed booty for given recipes"
  | is datadirDesc  = "X marks the spot o' yer config file"
  | is progDesc     = "Yarr, this be Herm's! Type \"--help\" if yer a landlubber."
  | otherwise       = str
  where is = (str ==)
