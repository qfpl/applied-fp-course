{ mkDerivation, attoparsec, base, bytestring, doctest, hspec
, http-types, lens, mmorph, mtl, optparse-applicative, semigroups
, sqlite-simple, sqlite-simple-errors, stdenv, tasty, tasty-hunit
, text, time, transformers, waargonaut, wai, wai-extra, warp
}:
mkDerivation {
  pname = "applied-fp-course";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring http-types lens mtl optparse-applicative
    semigroups sqlite-simple sqlite-simple-errors text time
    transformers waargonaut wai warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring doctest hspec http-types mmorph mtl semigroups
    tasty tasty-hunit text transformers wai wai-extra
  ];
  description = "Simplest of web apps for educational purposes";
  license = stdenv.lib.licenses.bsd3;
}
