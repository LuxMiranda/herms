{ pkgs ? import <nixpkgs> { } }:

# We've pinned version 18.03 of nixpkgs for reproducible builds.

# See this link for a tutorial:
# https://github.com/Gabriel439/haskell-nix/tree/master/project0

import (pkgs.fetchFromGitHub {
  owner  = "NixOS";
  repo   = "nixpkgs";
  rev    = "19.03";
  sha256 = "0q2m2qhyga9yq29yz90ywgjbn9hdahs7i8wwlq7b55rdbyiwa5dy";
}) { }
