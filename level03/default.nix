{ mkDerivation, base, stylish-haskell, wai, warp, http-types, aeson, optparse-applicative, doctest, stdenv }:
mkDerivation {
  pname = "level03";
  version = "0.1.0.0";
  sha256 = "0";
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base wai warp http-types optparse-applicative aeson doctest
  ];
  description = "Simplest of web apps";
  license = stdenv.lib.licenses.bsd3;
}
