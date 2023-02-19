# Herm's

[![Hackage](https://img.shields.io/hackage/v/herms.svg)](https://hackage.haskell.org/package/herms)
[![stackage LTS
package](http://stackage.org/package/herms/badge/lts)](http://stackage.org/lts/package/herms)
![Travis build](https://api.travis-ci.org/JackMiranda/herms.svg?branch=master)

HeRM's: a Haskell-based Recipe Manager (yes, food recipes) for the command line.

![Herm's Interface](https://i.imgur.com/u9fPapw.jpg)

**Table of Contents**

- [Herm's](#herms)
    - [Features](#features)
        - [What's new](#whats-new)
        - [Supported languages](#supported-languages)
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
- Import and export recipes as JSON or YAML
- Generate shopping lists
- Keep track of recipes with tags
- Set default unit systems, serving sizes, language, and recipe (see
  [the section on configuration](#configuring-herms-and-managing-recipe-files))

#### What's new:

Version v2.0 is out! This is the first release for which pre-built binaries are
available to download on Github. There were many, many great changes in 2.0, so
thanks to everyone for the contributions!

See [the changelog](./CHANGELOG.md) for more details.

#### Supported languages

- català
- English
- Español
- français
- Pirate (English)
- português

### Contributing

Herms is very actively maintained and welcomes new contributions, whether in
code, issues, documentation, translations, or feature suggestions!

Please see [Contributing.md](./Contributing.md) for more information.


### Installation

At the moment, Herm's can only be installed via
[stack](https://docs.haskellstack.org/en/stable/README/) or
[cabal](https://www.haskell.org/cabal/), but standalone binaries are in the
works!

If you're interested developing/hacking Herm's instead of just using it, see
[Contributing.md](./Contributing.md) instead.


##### Via Stack

```
stack update
stack install herms
```

##### Manually cloning and installing from source with Stack

```
git clone https://github.com/LuxMiranda/herms
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

See [./nix/README.md](./nix/README.md).


### Usage


#### Command-line interface

Herm's has a command-line interface!

Below is the exhaustive list of all commands and their functionalities. Take a gander!

```
Usage: herms [-v|--version] COMMAND
  HeRM's: a Haskell-based Recipe Manager. Type "herms --help" for options

  Available options:
    -h,--help                Show this help text
    -v,--version             Show version

    Available commands:
      list                     list recipes
      view                     view the particular recipes
      add                      add a new recipe (interactively)
      edit                     edit a recipe
      import                   import a recipe file
      export                   export recipes to stdout
      remove                   remove the particular recipes
      shopping                 generate a shopping list for given recipes
      datadir                  show locations of recipe and config files
      find                     find all matching strings within recipe book
                               (supports extended regex)
```


#### Configuring Herm's and managing recipe files

Herm's stores files in the following locations:

- The configuration file, `config.hs` in the XDG configuration directory,
  typically `~/.config/herms` on most Linux systems

- The recipes file, `recipes.yaml` in the XDG data directory,
  typically `~/.local/share/herms` on most Linux systems

To see where these are stored on your system, run ``herms datadir``.

``config.hs`` is a pseudo-Haskell-source-code file with several options for
configuring the behaviour of Herm's. It currently supports the following
options:

- `defaultUnit` : The default unit system to show recipes in. Options:
  `Imperial`, `Metric`, `None`. Setting to `None` will simply show recipes in
  whatever unit system they're stored in.
- `defaultServingSize` : Default serving size to calculate when showing recipes.
  Can be any non-negative integer; set to `0` for no default. This can be useful
  when you're always cooking for the same number of people!
- `recipesFile` : The recipes file to use. This option currently supports
  relative (but not absolute) location, as well; if your data directory is
  `~/herms/data`, for example, and you want a recipe file in your home directory
  called `~/GrandmasHugeCookbook.yaml`, set this option to
  `"../../GrandmasHugeCookbook.yaml"`.
- `language` : Human language to use. See above supported languages.


---

In honor of Logan, Utah's greatest Breakfast & Brunch.

![Herm's Inn](https://images.duckduckgo.com/iu/?u=https%3A%2F%2Firs2.4sqi.net%2Fimg%2Fgeneral%2F600x600%2F803_DzmDgevV4Yw5OrVsh9c4iaE7Bx8aSA0AY7y4L5Um7Qg.jpg&f=1)
