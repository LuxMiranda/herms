ConfigInfo
{
-- Default unit system to show recipes in.
-- OPTIONS: Imperial, Metric, None
  defaultUnit = None

-- Default serving size to calculate when showing recipes.
-- Set to 0 for no default.
, defaultServingSize = 5

-- Recipes file name relative to the user's XDG data dir, generally
-- ~/.local/share/herms on Linux systems. Default is "recipes.yaml".
, recipesFile = "recipes.yaml"
, language = "english"
}
