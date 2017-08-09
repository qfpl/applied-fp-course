{ mkDerivation, base, wai, warp, http-types, stdenv }:
mkDerivation {
  pname = "level01";
  version = "0.1.0.0";
  sha256 = "0";
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base wai warp http-types ];
  description = "Simplest of web apps";
  license = stdenv.lib.licenses.bsd3;
}
