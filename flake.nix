{
  description = "QFPL Applied FP Course";

  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgs = import inputs.nixpkgs { inherit system; };
        applied-fp-course = { returnShellEnv ? false }:
          nixpkgs.haskellPackages.developPackage {
            inherit returnShellEnv;
            root = ./.;

            overrides = final: prev: with nixpkgs.haskell.lib; {
              # hw-json-simd = dontCheck (unmarkBroken prev.hw-json-simd);
              # hw-json-standard-cursor = doJailbreak prev.hw-json-standard-cursor;
              # natural = dontCheck (unmarkBroken prev.natural);
              sqlite-simple-errors =
                unmarkBroken (doJailbreak prev.sqlite-simple-errors);
              # waargonaut = dontCheck (doJailbreak prev.waargonaut);
            };
          };
      in
      {
        devShell =
          (applied-fp-course { returnShellEnv = true; }).overrideAttrs
            (oldAttrs: {
              buildInputs = oldAttrs.buildInputs ++ [
                nixpkgs.cabal-install
                nixpkgs.ghcid
                nixpkgs.sqlite

                nixpkgs.stylish-haskell
              ];
            });
      });
}
