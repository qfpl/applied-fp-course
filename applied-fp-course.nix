{ mkDerivation, aeson, base, bytestring, doctest, hspec, hspec-wai
, http-types, mtl, optparse-applicative, semigroups, sqlite-simple
, sqlite-simple-errors, stdenv, tasty, tasty-hunit, text, time
, transformers, wai, wai-extra, warp
}:
mkDerivation {
  pname = "applied-fp-course";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring http-types mtl optparse-applicative
    semigroups sqlite-simple sqlite-simple-errors text time
    transformers wai warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring doctest hspec hspec-wai http-types mtl tasty
    tasty-hunit text wai wai-extra
  ];
  description = "Simplest of web apps for educational purposes";
  license = stdenv.lib.licenses.bsd3;
}
