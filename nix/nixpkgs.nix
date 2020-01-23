let
  nixpkgsPin = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  pinnedPkgs = builtins.fetchTarball {
    url = "${nixpkgsPin.url}/archive/${nixpkgsPin.rev}.tar.gz";
    inherit (nixpkgsPin) sha256;
  };
in
import pinnedPkgs {}
