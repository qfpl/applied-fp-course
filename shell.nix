{ compiler ? "default"
}:
let
  inherit (import <nixpkgs> {}) fetchFromGitHub;

  nixpkgs = fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs-channels";
    rev    = "4f3446f29910d21eb0fb942bd03091b089cdad63";
    sha256 = "0dqjkhhhckp881mns69qxn4dngcykal1gqrpaf9qy2vja4i41ay5";
  };

  pkgs = import nixpkgs {};

  # Grab our course derivation
  course = import ./. { nixpkgs = pkgs; inherit compiler; };

  # Override the basic derivation so we can have a more fully feature
  # environment for hacking on the course material
  courseDevEnv = (pkgs.haskell.lib.addBuildTools course
    [ # Include the SQLite Database application
      pkgs.sqlite

      # 'ghcid' auto reloading tool
      pkgs.haskellPackages.ghcid
    ]
  # We don't want nix to build the thing, we want the environment so we can
  # build the thing.
  ).env;

in
  # Fly, my pretties!
  courseDevEnv
