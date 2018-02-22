{ mkDerivation, aeson, async, base, bytestring, containers
, directory, doctest, hedgehog, hspec, hspec-wai, http-types
, mmorph, mtl, optparse-applicative, semigroups, sqlite-simple
, sqlite-simple-errors, stdenv, tasty, tasty-hedgehog, text, time
, transformers, wai, wai-extra, warp
}:
mkDerivation {
  pname = "level42";
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
    aeson async base bytestring containers directory doctest hedgehog
    hspec hspec-wai http-types mmorph mtl tasty tasty-hedgehog text
    transformers wai wai-extra warp
  ];
  description = "Simplest of web apps";
  license = stdenv.lib.licenses.bsd3;
}
