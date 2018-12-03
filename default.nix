{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default"
}:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  waarg = import ./nix/waargonaut.nix;
  waarg-deps = import "${waarg}/waargonaut-deps.nix";

  modifiedHaskellPackages = haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions
      (old.overrides or (_: _: {}))
      (self: super: (waarg-deps pkgs self super) // {
        waargonaut = self.callPackage (import "${waarg}/waargonaut.nix") {};
      });
  });

  drv = modifiedHaskellPackages.callPackage ./applied-fp-course.nix {};

in
  drv
