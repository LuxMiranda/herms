module Lang.Strings where

--------------------------
---- Add Recipe TUI ------
--------------------------

-- *** Note: Whitespace is important!!! ***

tuiTitle       = "                                    Herm's - Add a recipe"
tuiName        = "          Name: "
tuiDesc        = "   Description: "
tuiServingSize = "  Serving size: "
tuiHeaders     = "                  qty.   unit               name                attribute"
tuiIngrs       = "  Ingredients: \n(one per line)  "
tuiDirs        = "   Directions: \n(one per line)  "
tuiTags        = "          Tags: "
tuiHelp1       = "                      Tab / Shift+Tab           - Next / Previous field"
tuiHelp2       = "                      Ctrl + <Arrow keys>       - Navigate fields"
tuiHelp3       = "                      [Meta or Alt] + <h-j-k-l> - Navigate fields"
tuiHelp4       = "                      Esc                       - Save or Cancel"

--------------------
-- Recipe Headers --
--------------------

headerServs = "\nServings: "
headerIngrs = "\nIngredients:\n"

--------------------------
-- Interactive messages --
--------------------------

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

--------------------------
--   Options and Flags  --
--------------------------

group      = "group"
groupShort = 'g'
groupDesc  = "group recipes by tags"

nameOnly      = "name-only"
nameOnlyShort = 'n'
nameOnlyDesc  = "show only recipe names"

tags        = "tags"
tagsMetavar = "TAGS"
tagsDesc    = "show recipes with particular tags"

serving        = "serving"
servingShort   = 's'
servingDesc    = "specify serving size when viewing"
servingMetavar = "INT"

format        = "format"
formatShort   = 'f'
formatDesc    = "format for export"
formatMetavar = "FORMAT"

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

--------------------------
-- Commands             --
--------------------------

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

export     = "export"
exportDesc = "export recipes to stdout"

remove     = "remove"
removeDesc = "remove the particular recipes"

shopping     = "shopping"
shoppingDesc = "generate a shopping list for given recipes"

datadir     = "datadir"
datadirDesc = "show locations of recipe and config files"

version      = "version"
versionShort = 'v'
versionDesc  = "Show version"

find = "find"
findShort = 'r' -- since 'f' is already used. Think of it as "regex"
findDesc = "find all matching strings within recipe book (supports extended regex)"

--------------------------
--     Misc             --
--------------------------

progDesc = "HeRM's: a Haskell-based Recipe Manager. Type \"herms --help\" for options"
unsupportedFormat = "Unsupported format: "
supportedFormats  = "Supported formats are: json yaml"
