{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc801" , stdenv ? nixpkgs.stdenv}:

let
  siteBuilder = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./foo.nix { };
in
  stdenv.mkDerivation rec {
    name = "nmattia-com-builder";

    src = if stdenv.lib.inNixShell then null else ./.;

    buildPhase = ''
      ${siteBuilder}/bin/site build
    '';

    installPhase = ''
      mkdir -p $out
      ${nixpkgs.pkgs.rsync}/bin/rsync -rts _site/ $out
    '';
  }
