{ mkDerivation, base, wai, warp, http-types, aeson, optparse-applicative,
  hspec, hspec-wai, mtl, sqlite-simple, sqlite-simple-errors
  , stdenv
}:
mkDerivation {
  pname = "level06";
  version = "0.1.0.0";
  sha256 = "0";
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base wai warp http-types optparse-applicative aeson
    hspec hspec-wai mtl sqlite-simple sqlite-simple-errors
  ];
  description = "Simplest of web apps";
  license = stdenv.lib.licenses.bsd3;
}
