{ mkDerivation, stdenv, base, wai, warp, http-types }:

mkDerivation {
  pname = "level01";
  src = ./.;
  version = "0.1.0.0";
  sha256 = "0";
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base wai warp http-types ];
  description = "Simplest of web apps";
  license = stdenv.lib.licenses.bsd3;
}
