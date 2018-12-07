{ mkDerivation, attoparsec, base, bytestring, contravariant
, doctest, hedgehog, http-types, lens, mtl, old-locale
, optparse-applicative, semigroups, sqlite-simple
, sqlite-simple-errors, stdenv, tasty, tasty-hedgehog, tasty-hunit
, tasty-wai, text, time, transformers, waargonaut, wai, wai-extra
, warp
}:
mkDerivation {
  pname = "applied-fp-course";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring contravariant http-types lens mtl
    old-locale optparse-applicative semigroups sqlite-simple
    sqlite-simple-errors text time transformers waargonaut wai warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring doctest hedgehog http-types mtl semigroups tasty
    tasty-hedgehog tasty-hunit tasty-wai text transformers wai
    wai-extra
  ];
  description = "Simplest of web apps for educational purposes";
  license = stdenv.lib.licenses.bsd3;
}
