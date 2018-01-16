{ mkDerivation, aeson, base, bytestring, doctest, hspec, hspec-wai
, http-types, optparse-applicative, stdenv, text, wai, wai-extra
, warp
}:
mkDerivation {
  pname = "level03";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring http-types optparse-applicative text wai warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring doctest hspec hspec-wai wai wai-extra
  ];
  description = "Simplest of web apps";
  license = stdenv.lib.licenses.bsd3;
}
