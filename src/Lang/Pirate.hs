module Lang.Pirate where

import Lang.Strings

pirate string = case string of
                  progDesc    -> "Yarr, this be Herm's! Type \"--help\" if yer a landlubber."
                  _           -> string

