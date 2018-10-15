{ pkgs ? import <nixpkgs> { } }:

# We've pinned version 18.03 of nixpkgs for reproducible builds.

# See this link for a tutorial:
# https://github.com/Gabriel439/haskell-nix/tree/master/project0

import (pkgs.fetchFromGitHub {
  owner  = "NixOS";
  repo   = "nixpkgs";
  rev    = "18.03";
  sha256 = "0hk4y2vkgm1qadpsm4b0q1vxq889jhxzjx3ragybrlwwg54mzp4f";
}) { }
