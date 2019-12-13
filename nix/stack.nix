{ nixpkgs ? ./pinned-pkgs.nix }:

let
  pkgs = import nixpkgs {};
in
  pkgs.haskell.lib.buildStackProject {
    ghc = pkgs.haskell.compiler.ghc865;
    name = "herms";
  }
