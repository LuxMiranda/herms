{ pkgs ? import <nixpkgs> { } }:

# We've pinned nixpkgs for reproducible builds.

# See this link for a tutorial:
# https://github.com/Gabriel439/haskell-nix/tree/master/project0

import (pkgs.fetchFromGitHub {
  owner  = "NixOS";
  repo   = "nixpkgs";
  rev    = "cc6cf0a96a627e678ffc996a8f9d1416200d6c81";
  sha256 = "1srjikizp8ip4h42x7kr4qf00lxcp1l8zp6h0r1ddfdyw8gv9001";
}) { }
