# Herm's

HeRM's: a Haskell-based Recipe Manager (yes, food recipes) for the command line with big dreams of one day having a GUI.

![Herm's Inn](https://images.duckduckgo.com/iu/?u=https%3A%2F%2Firs2.4sqi.net%2Fimg%2Fgeneral%2F600x600%2F803_DzmDgevV4Yw5OrVsh9c4iaE7Bx8aSA0AY7y4L5Um7Qg.jpg&f=1)

In honor of Logan's greatest Breakfast & Brunch

### Features
- Add recipes! :)
- Look at recipes! :D
- Edit recipes! :DD
- Serving size adjustment! :DDD
- Remove recipes :(

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
        herms list                              list recipes

        herms view {index or "Recipe Name"}     view a particular recipe

        herms add                               add a new recipe (interactive)

        herms edit {index or "Recipe Name"}     edit a recipe

        herms remove {index or "Recipe Name"}   remove a particular recipe

        herms help                              display this help

OPTIONS
        -s{num}                                 specify serving size when viewing.
                                                E.g., 'herms view -s2 {recipe}' for two servings
```
