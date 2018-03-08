{ mkDerivation, base, hakyll, stdenv }:
mkDerivation {
  pname = "nmattia-site-builder";
  version = "0.1.0.0";
  src = ./builder;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hakyll ];
  license = stdenv.lib.licenses.free;
}
