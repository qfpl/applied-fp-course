{ nixpkgs ? import ./nix/nixpkgs.nix
, compiler ? "default"
}:
let
  pkgs = import nixpkgs {
    overlays = (import ./course-overlays.nix);
  };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage ./applied-fp-course.nix {};

in
  drv
