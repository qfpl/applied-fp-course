let
  sources = {
    tasty-wai = import ./nix/tasty-wai.nix;
    waarg = import ./nix/waargonaut.nix;
  };

  waarg-deps = import "${sources.waarg}/waargonaut-deps.nix";

  course = self: super: {
    haskellPackages = super.haskellPackages.override (old: {
      overrides = super.lib.composeExtensions (old.overrides or (_: _: {})) (hself: hsuper: {
        tasty-wai = hself.callCabal2nix "tasty-wai" sources.tasty-wai {};
        waargonaut = hself.callCabal2nix "waargonaut" sources.waarg {};
      });
    });
  };
in
  [ waarg-deps course ]
  