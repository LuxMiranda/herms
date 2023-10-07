module Lang.Catalan where

import Lang.Strings

catalan :: String -> String
catalan str
  | is tuiTitle = "                             Herm's - Afegir/Editar una recepta"
  | is tuiName = "        Nom:   "
  | is tuiDesc = "   Descripció: "
  | is tuiServingSize = "     Porcions: "
  | is tuiHeaders = "                quant. unitat         nom                       atribut"
  | is tuiIngrs = "  Ingredients: \n(un per línia)  "
  | is tuiDirs = " Instruccions: \n(una per línia)  "
  | is tuiTags = "     Etiquetes: "
  | is tuiHelp1 = "                      Tab / Shift+Tab           - Següent / Camp anterior"
  | is tuiHelp2 = "                      Ctrl + <cursor>           - Navegar camps"
  | is tuiHelp3 = "                      [Meta o Alt] + <h-j-k-l>  - Navegar camps"
  | is tuiHelp4 = "                      Esc                       - Desar o Cancel·lar"
  | is headerServs = "\nRacions: "
  | is headerIngrs = "\nIngredients:\n"
  | is saveRecipeYesNoEdit = "¿Desar la recepta? (S)í  (N)o (E)ditar"
  | is y = "s"
  | is yCap = "S"
  | is n = "n"
  | is nCap = "N"
  | is e = "e"
  | is eCap = "E"
  | is recipeSaved = "Recepta desada!"
  | is changesDiscarded = "Canvis descartats."
  | is badEntry = "Si et plau, escriu ÚNICAMENT 's', 'n' o 'e'"
  | is doesNotExist = " no existeix\n"
  | is nothingToImport = "Res per a importar"
  | is importedRecipes = "Receptes importades:"
  | is metric = "mètric"
  | is imperial = "imperial"
  | is more = " [més]"
  | is capTags = "Etiquetes"
  | is removingRecipe = "Eliminant recepta: "
  | is group = "grup"
  | is [groupShort] = "g"
  | is groupDesc = "agrupar receptes per etiquetes"
  | is nameOnly = "només-nom"
  | is [nameOnlyShort] = "n"
  | is nameOnlyDesc = "mostrar únicament els noms de les receptes"
  | is tags = "etiquetes"
  | is tagsMetavar = "ETIQUETES"
  | is tagsDesc = "mostrar receptes amb etiquetes específiques"
  | is serving = "ració"
  | is [servingShort] = "p"
  | is servingDesc = "especificar la mida de la porció al visualitzar"
  | is servingMetavar = "INT"
  | is step = "pas"
  | is [stepShort] = "p"
  | is stepDesc = "Si es visualitza pas a pas"
  | is convert = "convertir"
  | is [convertShort] = "c"
  | is convertDesc = "Converteix les unitats de la recepta a mètric o imperial."
  | is convertMetavar = "CONV_UNIT"
  | is none = "Cap"
  | is recipeNameMetavar = "NOM_RECEPTA"
  | is recipeNameDesc = "índex o nom de la recepta"
  | is fileNameMetavar = "NOM_FITXER"
  | is fileNameDesc = "nom del fitxer"
  | is list = "llista"
  | is listDesc = "llistar receptes"
  | is view = "visualitzar"
  | is viewDesc = "visualitzar les receptes específiques"
  | is add = "nova"
  | is addDesc = "agregar una nova recepta (interactivament)"
  | is edit = "editar"
  | is editDesc = "editar un recepta"
  | is import' = "importar"
  | is importDesc = "importar un fitxer de recepta"
  | is export = "exportar"
  | is exportDesc = "exportar receptes a stdout"
  | is remove = "eliminar"
  | is removeDesc = "eliminar les receptes específiques"
  | is shopping = "compres"
  | is shoppingDesc = "generar una llista de compra per a les receptes especificades"
  | is datadir = "datadir"
  | is datadirDesc = "mostrar la ubicació dels fitxers de recepta i configuració"
  | is version = "versió"
  | is [versionShort] = "v"
  | is versionDesc = "Visualitzar versió"
  | is progDesc = "HeRM's: Un administrador de receptes en Haskell. Escriu \"herms --help\" per a veure les opcions"
  | otherwise = str
  where
    is = (str ==)