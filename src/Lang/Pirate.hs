module Lang.Pirate where

import Lang.Strings

pirate :: String -> String
pirate str
  | is tuiTitle       = "                                  Herm's - Drop the anchor!"
  | is tuiName        = "  Giv'r a name: "
  | is tuiDesc        = "    Describin': "
  | is tuiServingSize = "  Servin' size: "
  | is tuiIngrs       = "     Reagents: \n(one per line)  "
  | is tuiHelp1       = "                      Tab / Shift+Tab           - Next / Previous bay"
  | is tuiHelp2       = "                      Ctrl + <Arrow keys>       - Navigate the sea"
  | is tuiHelp3       = "                      [Meta or Alt] + <h-j-k-l> - Navigate the sea"
  | is headerServs    = "\nServin's: "
  | is headerIngrs    = "\nReagents:\n"
  | is saveRecipeYesNoEdit = "Bury the booty? (Y)ar!  (N)ay  (E)dit"
  | is recipeSaved      = "Ahoy! Recipe on-boarded!"
  | is changesDiscarded = "Changes discARRded."
  | is badEntry         = "Argh! Please enter ONLY 'y', 'n' or 'e'"
  | is doesNotExist     = " don't exist, mate\n"
  | is nothingToImport  = "Ain't nothin' to import"
  | is importedRecipes  = "On-boarded recipes:"
  | is more             = " [marr]"
  | is removingRecipe   = "Recipe walked the plank: "
  | is listDesc         = "list yer recipes"
  | is viewDesc         = "gander at yer particular recipes"
  | is removeDesc       = "show a recipe to the plank"
  | is shoppingDesc     = "make a list o' needed booty for given recipes"
  | is datadirDesc      = "X marks the spot o' yer config file"
  | is progDesc         = "Yarr, this be Herm's! Type \"--help\" if yer a landlubber."
  | otherwise           = str
  where is = (str ==)
