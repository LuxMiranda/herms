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

### What's new:
- Set default unit systems, serving sizes, and recipe file path in ``config.hs``!
    - Coming soon: Multi-language support
- Added support for multi-word tags
- Check your Herm's version with ``-v`` or ``--version``
- Herm's is now on Stackage!

### Installation

#### PATH setup

Firstly, make sure that the following is added to your PATH:

```
~/.cabal/bin
```

#### Download and install

At the moment, Herm's can only be compiled from source, but binaries are in the works!

##### Manually cloning and installing from source with Stack _(recommended)_:

```
git clone https://github.com/JackKiefer/herms
cd herms
stack update
stack install
```

##### Via Hackage and Cabal:

```
cabal update
cabal install herms
```

##### Manually with Cabal:

Your milage may vary with dependency resolution

```
git clone https://github.com/JackKiefer/herms
cd herms
cabal update
cabal install -j
```

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

        -v|--version                                   Show version

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
