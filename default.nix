{ pkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  cabal2nix = name: path: pkgs.runCommand name {} ''
    ${pkgs.cabal2nix}/bin/cabal2nix ${path} > $out
  '';

  haskellPackages = (if compiler == "default"
                     then pkgs.haskellPackages
                     else pkgs.haskell.packages.${compiler}).override {
    overrides = self: _: {
      ares = self.callPackage (cabal2nix "ares.nix" ./.) {};
    };
  };
in

haskellPackages.ares
