{ pkgs ? import ./pinned-pkgs.nix { } }:

with pkgs;
let haskellPackages = haskell.packages.ghc881;
    herms = with haskellPackages; callPackage ./default.nix { };
in stdenv.mkDerivation {
  name = "herms-dev";
  version = "0.1";

  # Ensure Cabal doesn't pick up on the user's already-built libraries
  shellHook = ''
    export HOME=$(mktemp -d)
  '';

  src = lib.sourceFilesBySuffices ../. [ ".cabal" ".hs" ];
  buildInputs =  [

    (haskellPackages.ghcWithHoogle (hpkgs: with hpkgs; [
      # Add extra library dependencies here
    ] ++ herms.buildInputs ++ herms.propagatedBuildInputs))
    hlint
    cabal-install

    # General development
    git
  ];
}
