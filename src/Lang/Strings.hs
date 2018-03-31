module Lang.Strings where

{--------------------------
 -- Interactive messages --
 --------------------------}

saveRecipeYesNoEdit = "Save recipe? (Y)es  (N)o  (E)dit"

y    = "y" 
yCap = "Y"

n    = "n" 
nCap = "N"

e    = "e" 
eCap = "E"

recipeSaved = "Recipe saved!"

changesDiscarded = "Changes discarded."

badEntry = "Please enter ONLY 'y', 'n' or 'e'"

doesNotExist = " does not exist\n"

nothingToImport = "Nothing to import"

importedRecipes = "Imported recipes:"

metric = "metric"

imperial = "imperial"

more = " [more]"

capTags = "Tags"

removingRecipe = "Removing recipe: "

{--------------------------
 --   Options and Flags  --
 --------------------------}
 
group      = "group"
groupShort = 'g'
groupDesc  = "group recipes by tags"

nameOnly      = "name-only"
nameOnlyShort = 'n'
nameOnlyDesc  = "show only recipe names"

tags        = "tags"
tagsMetavar = "TAGS"
tagsDesc    = "show recipws with particular tags"

serving        = "serving"
servingShort   = 's'
servingDesc    = "specify serving size when viewing"
servingMetavar = "INT"

step      = "step"
stepShort = 't'
stepDesc  = "Whether to show one step at a time"

convert        = "convert"
convertShort   = 'c'
convertDesc    = "Converts recipe units to either metric or imperial."
convertMetavar = "CONV_UNIT"

none = "none"

recipeNameMetavar = "RECIPE_NAME"
recipeNameDesc    = "index or Recipe name"

fileNameMetavar = "FILE_NAME"
fileNameDesc    = "file name"

{--------------------------
 -- Commands             --
 -------------------------}
 
list     = "list"
listDesc = "list recipes"

view     = "view"
viewDesc = "view the particular recipes"

add     = "add"
addDesc = "add a new recipe (interactively)"

edit     = "edit"
editDesc = "edit a recipe"

import'    = "import"
importDesc = "import a recipe file"

remove     = "remove"
removeDesc = "remove the particular recipes"

shopping     = "shopping"
shoppingDesc = "generate a shopping list for given recipes"

datadir     = "datadir"
datadirDesc = "show location of recipe and config files"

version      = "version"
versionShort = 'v'
versionDesc  = "Show version"

{--------------------------
 --     Other            --
 --------------------------}
 
progDesc = "HeRM's: a Haskell-based Recipe Manager. Type \"herms --help\" for options"
