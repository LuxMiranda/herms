{ pkgs ? import ./pinned-pkgs.nix { } }:

pkgs.haskellPackages.callCabal2nix "herms" ../. { }