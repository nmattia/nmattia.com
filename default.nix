{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc801" , stdenv ? nixpkgs.stdenv}:

let
  siteBuilder = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./foo.nix { };
in
  stdenv.mkDerivation rec {
    name = "nmattia-com-builder";

    src = if stdenv.lib.inNixShell then null else ./.;

    buildPhase = ''
      export LANG=en_US.UTF-8 # this fixes the weird stack issue
      ${siteBuilder}/bin/site build
    '';

    installPhase = ''
      mkdir -p $out
      ${nixpkgs.pkgs.rsync}/bin/rsync -rts _site/ $out
      echo "All good!"
    '';
  }
