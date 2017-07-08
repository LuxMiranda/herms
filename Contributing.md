## Rules
 - Delint your code with hlint before submitting a pull request
 - Be courteous of folks' dietary decisions
 - Be loving for someone today and have fun!

## Development
We use stack around here to develop. Uhh, refer to [stack documentation.](https://docs.haskellstack.org/en/stable/GUIDE/)

## Definite feature plans

A lot of these are quite indepedent from each other. If one looks pretty fun to you, you could probably jump right in!

Once all of the **bolded** features are implemented, we're going to begin the proccess of uploading to Hackage!

### Easy squeezy
- Import recipe file
- Optional caloric / nutritional information
- **Add categories for recipes** (I'm thinking of categories as simply lists of recipes... That way, recipes can be part of multiple categories and deleting a category doesn't break your recipes) 
- Add better erroneous input handling
- Make yummy recipes and eat them

### May require elbow grease
- **Sort by tag**
- Edit recipes (including an **option to go back and edit** on the save/cancel prompt when adding a recipe)
- **Unit conversions**
- Translation into languages other than English
- "One step at a time" mode

### Are you crazy?!
- Parse and store a recipe given a URL to allrecipes.com, food.com, etc.
- Show available recipes given a list of ingredients
- A pretty GUI!

## Known bugs
- Passing multiple recipes into a single `herms remove` command doesn't quite behave as you'd expect it to! See discussion in [#4](https://github.com/JackKiefer/herms/pull/4)

## Other notes on design improvements
- Rewriting out the entire recipe file minus a single recipe every time you remove a recipe is fine for now, but will quickly become impractical when storing large numbers of recipes. We ought to find a better way of doing it! Unless I'm just crazy because even the most dedicated chefs might store only a few hundred recipes...
- There's a really gross thing where the entire removing function was copied over to a "silentRemove" function that does the exact same thing but without printing output. This was done to avoid restructuring the way that functions are dispatched. If we could figure out an elegent way to merge the two functions back together, that'd be great...
- Argument handling is also a bit janky at the moment. There's defintiely some room for improvement!
