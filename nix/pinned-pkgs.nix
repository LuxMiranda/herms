{ pkgs ? import <nixpkgs> { } }:

# We've pinned nixpkgs for reproducible builds.

# See this link for a tutorial:
# https://github.com/Gabriel439/haskell-nix/tree/master/project0

import (pkgs.fetchFromGitHub {
  owner  = "NixOS";
  repo   = "nixpkgs";
  rev    = "1c15955340811db41e8d37da18254c7510a07ea2";
  sha256 = "1m4x40i7sgwiv5w3zfcigjx39rs7adnnrabhi653x504y29hfglk";
}) { }
