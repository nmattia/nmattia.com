{ lib, mkDerivation, base, filepath, hakyll }:
mkDerivation {
  pname = "nmattia-site-builder";
  version = "0.1.0.0";
  src = lib.cleanSource ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hakyll filepath ];
  license = lib.licenses.free;
}
