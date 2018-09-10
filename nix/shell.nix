{ pkgs ? import ./pinned-pkgs.nix { } }:

with pkgs;
let herms = with haskellPackages; callPackage ./default.nix { };
in stdenv.mkDerivation {
  name = "herms-dev";
  version = "0.1";

  # Ensure Cabal doesn't pick up on the user's already-built libraries
  shellHook = ''
    export HOME=$(mktemp -d)
  '';

  src = lib.sourceFilesBySuffices ../. [ ".cabal" ".hs" ];
  buildInputs =  [

    (haskell.packages.ghc822.ghcWithPackages (hpkgs: with hpkgs; [

      # Nice for development
      hoogle
      hlint
      hindent
      ghcid
      # ghc-mod # Doesn't build with GHC843
      hasktags

      # Necessary
      cabal-install
    ] ++ herms.buildInputs ++ herms.propagatedBuildInputs))

    # General development
    git
    less
  ];
}
