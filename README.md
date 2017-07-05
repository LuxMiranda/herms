# Herm's

HeRM's: a Haskell-based Recipe Manager (yes, food recipes) for the command line with big dreams of one day having a GUI.

![Herm's Inn](https://images.duckduckgo.com/iu/?u=https%3A%2F%2Firs2.4sqi.net%2Fimg%2Fgeneral%2F600x600%2F803_DzmDgevV4Yw5OrVsh9c4iaE7Bx8aSA0AY7y4L5Um7Qg.jpg&f=1)

In honor of Logan's greatest Breakfast & Brunch

### Features
- Add recipes!
- Look at recipes!
- Remove recipes!
- Coming soon: serving size calculation!

### Installation

At the moment Herm's can only be compiled from source. You'll need GHC and cabal.

Run the following commands:
```
git clone https://github.com/JackKiefer/herms
cd herms
cabal update
cabal install
```

### Usage
```
herms list                  - list recipes
herms view "Recipe Name"    - view a particular recipe
herms add                   - add a new recipe (interactive)
herms remove "Recipe Name"  - remove a particular recipe
herms help                  - display this help
```
