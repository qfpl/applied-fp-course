let
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    tasty-wai-pinned = initialNixpkgs.pkgs.lib.importJSON ./tasty-wai.json;
    tasty-wai = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "tasty-wai";
      inherit (tasty-wai-pinned) rev sha256;
    };
  };
in
  sources.tasty-wai
