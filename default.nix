{ pkgs ? import <nixpkgs> {} }:

let
  siteBuilder = pkgs.haskellPackages.callPackage ./foo.nix { };
in
  pkgs.stdenv.mkDerivation rec {
    name = "nmattia-com-builder";

    src = pkgs.lib.cleanSourceWith 
      { filter = name: type:
          let baseName = baseNameOf (toString name);
          # filter out gh-pages build output
          in ! (  type == "directory" && baseName == "gh-pages"
              ||  type == "file" && baseName == ".gitignore") ;
        src = pkgs.lib.cleanSource ./.;
      };

    buildPhase = ''
      export LANG=en_US.UTF-8 # fixes charset issues
      ${siteBuilder}/bin/site build
    '';

    installPhase = ''
      mkdir -p $out
      ${pkgs.rsync}/bin/rsync -rts _site/ $out
      touch $out/.nojekyll
    '';
  }
