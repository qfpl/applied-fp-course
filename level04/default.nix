{ mkDerivation, base, wai, warp, http-types, aeson, optparse-applicative, hspec, hspec-wai, stdenv }:
mkDerivation {
  pname = "level04";
  version = "0.1.0.0";
  sha256 = "0";
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base wai warp http-types optparse-applicative aeson
    hspec hspec-wai
  ];
  description = "Simplest of web apps";
  license = stdenv.lib.licenses.bsd3;
}
