{ mkDerivation, base, stylish-haskell, wai, warp, http-types, yaml-config, aeson, optparse-applicative, stdenv }:
mkDerivation {
  pname = "level03";
  version = "0.1.0.0";
  sha256 = "0";
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base wai warp http-types yaml-config optparse-applicative aeson
  ];
  description = "Simplest of web apps";
  license = stdenv.lib.licenses.bsd3;
}
