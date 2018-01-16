{ mkDerivation, aeson, base, bytestring, doctest, hspec, hspec-wai
, http-types, mtl, optparse-applicative, sqlite-simple, semigroups
, sqlite-simple-errors, stdenv, text, time, wai, wai-extra, warp
, transformers
}:
mkDerivation {
  pname = "level06";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring http-types mtl optparse-applicative
    sqlite-simple sqlite-simple-errors text time wai warp semigroups
    transformers
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring doctest hspec hspec-wai wai wai-extra
  ];
  description = "Simplest of web apps";
  license = stdenv.lib.licenses.bsd3;
}
