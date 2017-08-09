{ mkDerivation, base, wai, warp, http-types, yaml-config, stdenv }:
mkDerivation {
  pname = "level02";
  version = "0.1.0.0";
  sha256 = "0";
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base wai warp http-types yaml-config
  ];
  description = "Simplest of web apps";
  license = stdenv.lib.licenses.bsd3;
}
