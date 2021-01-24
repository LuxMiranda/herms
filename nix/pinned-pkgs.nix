{ pkgs ? import <nixpkgs> { } }:

# We've pinned nixpkgs for reproducible builds.

# See this link for a tutorial:
# https://github.com/Gabriel439/haskell-nix/tree/master/project0

import (pkgs.fetchFromGitHub {
  owner  = "NixOS";
  repo   = "nixpkgs";
  rev    = "cd63096d6d887d689543a0b97743d28995bc9bc3";
  sha256 = "1wg61h4gndm3vcprdcg7rc4s1v3jkm5xd7lw8r2f67w502y94gcy";
}) { }
