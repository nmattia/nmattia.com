{ pkgs ? import <nixpkgs> {} }:

let
  siteBuilder = pkgs.haskellPackages.callPackage ./builder { inherit (pkgs) lib; };
  resume = import (
    pkgs.fetchFromGitHub
      { owner = "nmattia";
        repo = "resume";
        rev = "c9898b136e2f2e3a35e924a1d23a83cefd3ed38e";
        sha256 = "0j14ihwqad138vzdmjyf0dgks0yfgnxmxpiz0nv68vjqlcchrjbs";
      }) { inherit pkgs; };

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
      cp ${resume}/resume.pdf resume.pdf
      cp ${resume}/resume.html resume.html
      ${siteBuilder}/bin/site build
    '';

    installPhase = ''
      mkdir -p $out
      ${pkgs.rsync}/bin/rsync -rts _site/ $out
      touch $out/.nojekyll
    '';
  }
