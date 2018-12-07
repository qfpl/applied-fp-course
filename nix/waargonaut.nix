let
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    waargonaut-pinned = initialNixpkgs.pkgs.lib.importJSON ./waargonaut.json;
    waargonaut = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "waargonaut";
      inherit (waargonaut-pinned) rev sha256;
    };
  };
in
  sources.waargonaut
