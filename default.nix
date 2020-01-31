{ nixpkgs ? import ./nix/nixpkgs.nix
, compiler ? "default"
}:
let
  inherit (nixpkgs) pkgs;

  baseHaskellPackages = if compiler == "default"
                        then pkgs.haskellPackages
                        else pkgs.haskell.packages.${compiler};

  haskellPackages = baseHaskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      digit = unmarkBroken super.digit;
      hw-balancedparens = unmarkBroken super.hw-balancedparens_0_3_0_3;
      hw-bits = unmarkBroken super.hw-bits;
      hw-excess = unmarkBroken super.hw-excess;
      hw-json-standard-cursor = unmarkBroken super.hw-json-standard-cursor;
      hw-rankselect = doJailbreak (unmarkBroken super.hw-rankselect);
      hw-rankselect-base = unmarkBroken super.hw-rankselect-base;
      natural = doJailbreak (unmarkBroken super.natural);
      waargonaut = doJailbreak (unmarkBroken super.waargonaut);
    };
  };

  drv = haskellPackages.callPackage ./applied-fp-course.nix {};

in
  drv
