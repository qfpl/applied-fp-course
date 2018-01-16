{ mkDerivation, aeson, base, bytestring, doctest, http-types
, optparse-applicative, stdenv, text, wai, warp, semigroups
}:
mkDerivation {
  pname = "level05";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring http-types optparse-applicative text wai warp
    semigroups
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base doctest
  ];
  description = "Simplest of web apps";
  license = stdenv.lib.licenses.bsd3;
}
