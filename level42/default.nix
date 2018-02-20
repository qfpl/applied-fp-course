{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  myHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      hspec-wai = haskellPackages.callPackage ../../hspec-wai/hspec-wai.nix {};
    };
  };

  drv = myHaskellPackages.callPackage ./level42.nix {};

in
  if pkgs.lib.inNixShell then drv.env else drv
