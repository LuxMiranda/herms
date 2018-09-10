# Herm's

[![Hackage](https://img.shields.io/hackage/v/herms.svg)](https://hackage.haskell.org/package/herms)
[![stackage LTS
package](http://stackage.org/package/herms/badge/lts)](http://stackage.org/lts/package/herms)
![Travis build](https://api.travis-ci.org/JackKiefer/herms.svg?branch=master)

HeRM's: a Haskell-based Recipe Manager (yes, food recipes) for the command line.

![Herm's Interface](https://i.imgur.com/u9fPapw.jpg)

**Table of Contents**

- [Herm's](#herms)
    - [Features](#features)
        - [What's new](#whats-new)
    - [Contributing](#contributing)
    - [Installation](#installation)
        - [Manually cloning and installing from source with Stack](#manually-cloning-and-installing-from-source-with-stack)
        - [Via Hackage and Cabal](#via-hackage-and-cabal)
        - [Manually with Cabal](#manually-with-cabal)
        - [Manually with Nix](#manually-with-nix)
        - [Usage](#usage)
            - [Command-line interface](#command-line-interface)
            - [Configuring Herm's and managing recipe files](#configuring-herms-and-managing-recipe-files)

### Features
- Add recipes! :)
- Look at recipes! :D
- Edit recipes! :DD
- Serving size adjustment! :DDD
- Remove recipes :(
- View metric recipes in imperial units and vice-versa
- Import and export recipe files
- Generate shopping lists
- Keep track of recipes with tags
- Set default unit systems, serving sizes, language, and recipe (see
  [the section on configuration](#configuring-herms-and-managing-recipe-files))

#### What's new:

See [the changelog](./CHANGELOG.md) for migration support and more detailed discussion.

- Herm's supports YAML! Recipe files are now stored in YAML format by default
  (the old format is deprecated, see [the changelog](./CHANGELOG.md)).
  Furthermore, recipes can be imported and exported in JSON and YAML formats.
- *Breaking change*: Units now have a different internal representation.
  See [the changelog](./CHANGELOG.md) for migration instructions.

### Contributing

Herms is very actively maintained and welcomes new contributions, whether in
code, issues, documentation, translations, or feature suggestions!

Please see [Contributing.md](./Contributing.md) for more information.

### Installation

At the moment, Herm's can only be installed via [stack](https://docs.haskellstack.org/en/stable/README/) or [cabal](https://www.haskell.org/cabal/), but standalone binaries are in the works!

If you're interested developing/hacking Herm's instead of just using it, see [Contributing.md](Contibuting.md) instead.

##### Via Stack _(recommended)_

```
stack update
stack install herms
```

##### Manually cloning and installing from source with Stack

```
git clone https://github.com/JackKiefer/herms
cd herms
stack update
stack install
```

##### Via Hackage and Cabal

_Note_: Your mileage may vary with dependency resolution

```
cabal update
cabal install herms
```

##### Manually with Cabal

```
git clone https://github.com/JackKiefer/herms
cd herms
cabal update
cabal install
```

##### Manually with Nix

Herms works well with [cabal2nix](https://github.com/NixOS/cabal2nix). Put this in `default.nix`:
```nix
{ pkgs ? import <nixpkgs> { } }: 
pkgs.haskellPackages.callCabal2nix "herms" ./. { }
```
and build Herms with
```
nix build
```

### Usage

#### Command-line interface

Herm's has a pretty intuitive interface for users familiar with other command-line programs!

Below is the exhaustive list of all commands and their functionalities. Take a gander!

```
Usage:

        herms list [-g|--group] [--tags TAGS]           list recipes
                   [--name-only ]

        herms add                                       add a new recipe (interactively)

        herms edit RECIPE_NAME                          edit a recipe

        herms import FILE_NAME                          import a recipe file
        
        herms export DESTINATION                        export recipes to DESTINATION

        herms remove RECIPE_NAMES                       remove the particular recipes

        herms view RECIPE_NAMES [-s|--serving INT]      view the particular recipes
                                [-t|--step]
                                [-c|--convert CONV_UNIT]

        herms shopping RECIPE_NAMES [-s|--serving INT] generate shopping list for particular recipes

        herms datadir                                  show locations of recipe and config files

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

#### Configuring Herm's and managing recipe files

Herm's stores files in the following locations:

- The configuration file, `config.hs` in the XDG configuration directory,
  typically `~/.config/herms` on most Linux systems

- The recipes file, `recipes.herms` in the XDG data directory,
  typically `~/.local/share/herms` on most Linux systems

To see where these are stored on your system, run ``herms datadir``.

``config.hs`` is a pseudo-Haskell-source-code file with several options for configuring the behaviour of Herm's. It currently supports the following options:

- `defaultUnit` : The default unit system to show recipes in. Options: `Imperial`, `Metric`, `None`. Setting to `None` will simply show recipes in whatever unit system they're stored in.
- `defaultServingSize` : Default serving size to calculate when showing recipes. Can be any non-negative integer; set to `0` for no default. This can be useful when you're always cooking for the same number of people!
- `recipesFile` : The recipes file to use. This option currently supports relative (but not absolute) location, as well; if your data directory is `~/herms/data`, for example, and you want a recipe file in your home directory called `~/GrandmasHugeCookbook.herms`, set this option to `"../../GrandmasHugeCookbook.herms"`.
- `language` : Human language to use. Currently supported: ``"Français"``, ``"English"``, ``"Pirate"``


---

In honor of Logan, Utah's greatest Breakfast & Brunch.

![Herm's Inn](https://images.duckduckgo.com/iu/?u=https%3A%2F%2Firs2.4sqi.net%2Fimg%2Fgeneral%2F600x600%2F803_DzmDgevV4Yw5OrVsh9c4iaE7Bx8aSA0AY7y4L5Um7Qg.jpg&f=1)
