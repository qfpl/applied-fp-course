{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default"
}:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  sources = {
    tasty-wai = import ./nix/tasty-wai.nix;
    waarg = import ./nix/waargonaut.nix;
  };

  waarg-deps = import "${sources.waarg}/waargonaut-deps.nix";

  modifiedHaskellPackages = haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions
      (old.overrides or (_: _: {}))
      (self: super: (waarg-deps pkgs self super) // {
        tasty-wai = self.callCabal2nix "tasty-wai" sources.tasty-wai {};
        waargonaut = self.callCabal2nix "waargonaut" sources.waarg {};
      });
  });

  drv = modifiedHaskellPackages.callPackage ./applied-fp-course.nix {};

in
  drv
