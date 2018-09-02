## Rules
 - Delint your code with hlint before submitting a pull request
 - Be courteous of folks' dietary decisions
 - Be loving for someone today and have fun!

## Development
We use stack around here to develop. Uhh, refer to [stack documentation.](https://docs.haskellstack.org/en/stable/GUIDE/)
#### TL;DR
[Install stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/). In the `herms` directory:
```
stack setup
stack build
stack exec herms
```
Whim-bam!

## Feature/Improvement Ideas

A lot of these are quite independent from each other. If one looks pretty fun to you, you could probably jump right in!
Also take a look at the [Github issues](https://github.com/JackKiefer/herms/issues).

- Optional caloric / nutritional information
- Translation into languages other than English
- Add the following to the configuration file:
  - Language
  - Default listing options
  - Default recipe viewing options (Add some variety to how recipes can be displayed!)
  - Allow herms to create a recipe file specified if it doesn't already exist
- Search for recipes using keywords
  - Search just in titles, directions, ingredients, or any combination of the three
- Tab-complete when typing a recipe name

### Streeeetch goals

#### Actual Stretch Goals
- Parse and store a recipe given a URL to allrecipes.com, food.com, etc.
- A pretty (but optional) GUI!

#### Achievements in Computer Science
- Add an option to give up and just order pizza (without leaving the command line. [Take a look at this](https://github.com/fadein/sudo_make_me_a_sandwich) )
- Keep track of the costs of recipes using real-time price data (is this even practical?)

### Other Ideas

If you have another feature idea, please feel free to let us know either by submitting a GitHub issue or shooting an email!

## Known bugs

None at the moment!
