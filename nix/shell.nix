{ pkgs ? import ./pinned-pkgs.nix { } }:

with pkgs;
let herms = with haskellPackages; callPackage ./default.nix { };
in stdenv.mkDerivation {
  name = "herms-dev";
  version = "0.1";

  # Ensure Cabal doesn't pick up on the user's already-built libraries
  # shellHook = ''
  #   export HOME=$(mktemp -d)
  # '';

  src = lib.sourceFilesBySuffices ../. [ ".cabal" ".hs" ];
  buildInputs =  [
    (haskell.packages.ghc884.ghcWithPackages (hpkgs: with hpkgs; [
      # Put extras here
      ghcid
    ] ++ herms.buildInputs ++ herms.propagatedBuildInputs))
    cabal-install
    hlint
    ormolu
    git
  ];
}
