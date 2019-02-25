{ nixpkgs ? import ./nix/nixpkgs.nix # import <nixpkgs> {}
, compiler ? "default"
}:
let
  sources = {
    tasty-wai = import ./nix/tasty-wai.nix;
    waarg = import ./nix/waargonaut.nix;
  };

  waarg-deps = import "${sources.waarg}/waargonaut-deps.nix";

  course-overlay = self: super: {
    haskellPackages = super.haskellPackages.override (old: {
      overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (hself: hsuper: {
        tasty-wai = hself.callCabal2nix "tasty-wai" sources.tasty-wai {};
        waargonaut = hself.callCabal2nix "waargonaut" sources.waarg {};
      });
    });
  };

  pkgs = import nixpkgs {
    overlays = [ waarg-deps course-overlay ];
  };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  # sources = {
  #   tasty-wai = import ./nix/tasty-wai.nix;
  #   waarg = import ./nix/waargonaut.nix;
  # };

  # waarg-deps = import "${sources.waarg}/waargonaut-deps.nix";

  # modifiedHaskellPackages = haskellPackages.override (old: {
  #   overrides = pkgs.lib.composeExtensions
  #     (old.overrides or (_: _: {}))
  #     (self: super: (waarg-deps self super) // {
  #       tasty-wai = self.callCabal2nix "tasty-wai" sources.tasty-wai {};
  #       waargonaut = self.callCabal2nix "waargonaut" sources.waarg {};
  #     });
  # });

  drv = haskellPackages.callPackage ./applied-fp-course.nix {};

in
  drv
