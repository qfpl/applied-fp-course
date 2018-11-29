{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default"
}:
let
  inherit (nixpkgs) pkgs;

  # Grab our course derivation
  course = import ./. { inherit nixpkgs compiler; };

  # Override the basic derivation so we can have a more fully feature
  # environment for hacking on the course material
  courseDevEnv = (pkgs.haskell.lib.addBuildTools course
    [ # Include the SQLite Database application
      nixpkgs.sqlite

      # Optionally include the 'ghcid' auto reloading tool
      # nixpkgs.haskellPackages.ghcid
    ]
  # We don't want nix to build the thing, we want the environment so we can
  # build the thing.
  ).env;

in
  # Fly, my pretties!
  courseDevEnv
