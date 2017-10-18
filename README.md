# Herm's

![Travis build](https://api.travis-ci.org/JackKiefer/herms.svg?branch=master)

HeRM's: a Haskell-based Recipe Manager (yes, food recipes) for the command line.

![Herm's Interface](https://i.imgur.com/u9fPapw.jpg)

### Features
- Add recipes! :)
- Look at recipes! :D
- Edit recipes! :DD
- Serving size adjustment! :DDD
- Remove recipes :(
- View metric recipes in imperial units and vice-versa
- Import recipe files
- Generate shopping lists
- Keep track of recipes with tags

### Installation

At the moment Herm's can only be cloned and built locally. (But this shall change... _soon_)

You'll need GHC and cabal.

Run the following commands:
```
git clone https://github.com/JackKiefer/herms
cd herms
cabal update
cabal sandbox init
cabal install -j
```
To access Herm's from any directory, add the following directory to your PATH:
`/path/to/herms/.cabal-sandbox/bin`

### Usage
```
Usage:

        herms list [-g|--group] [--tags TAGS]           list recipes
                   [--name-only ]
 
        herms add                                       add a new recipe (interactively)

        herms edit RECIPE_NAME                          edit a recipe

        herms import FILE_NAME                          import a recipe file

        herms remove RECIPE_NAMES                       remove the particular recipes

        herms view RECIPE_NAMES [-s|--serving INT]      view the particular recipes
                                [-t|--step]
                                [-c|--convert CONV_UNIT] 

        herms shopping RECIPE_NAMES [-s|--serving INT] generate shopping list for particular recipes

Available options:

        -h|--help                                      Show this help text

        RECIPE_NAME                                    index or Recipe name

        --sort SORT_ORDER                              'tags' to sort by tags

        -s|--serving INT                               specify serving size when viewing.
                                                       E.g., 'herms view -s 2 {recipe}' for two servings

        -t|--step                                      View recipe in "one-step-at-a-time" mode
                                                       (press enter between each direction)

        -g|--group                                     group recipes by tags

        --tags TAGS                                    show recipes with particular flags

        --name-only                                    only list recipes by name

        -c|--convert CONV_UNIT                         view the recipe converted to imperial or metric
                                                       E.g., 'herms view 2 -c imperial'
```

In honor of Logan, Utah's greatest Breakfast & Brunch.

![Herm's Inn](https://images.duckduckgo.com/iu/?u=https%3A%2F%2Firs2.4sqi.net%2Fimg%2Fgeneral%2F600x600%2F803_DzmDgevV4Yw5OrVsh9c4iaE7Bx8aSA0AY7y4L5Um7Qg.jpg&f=1)
