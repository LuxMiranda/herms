# Herm's and Nix

This directory contains all you need to build and hack on Herm's with the
[Nix](https://nixos.org/nix) package manager.

## Building

Run the following in this directory: 
```bash
nix build -f .
```

Then you can run Herm's with 
```bash
./result/bin/herms
```

## Hacking

Run the following in this directory: 
```bash
nix-shell shell.nix
```
Then just use `cabal` to build Herm's!
