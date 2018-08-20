{ pkgs ? import ./nix }:

let
  siteBuilder = pkgs.haskellPackages.callPackage ./builder { inherit (pkgs) lib; };
in
  pkgs.stdenv.mkDerivation rec {
    name = "nmattia-com-builder";

    src = pkgs.lib.cleanSourceWith
      { filter = name: type:
          let baseName = baseNameOf (toString name);
          # filter out gh-pages build output and all irrelevant stuff
          in ! (  type == "directory" && baseName == "gh-pages"
              ||  type == "file" && baseName == ".gitignore") ;
        src = pkgs.lib.cleanSource ./.;
      };

    buildPhase = ''
      export LC_ALL=en_US.UTF-8
      export LANG=en_US.UTF-8
      export LANGUAGE=en_US.UTF-8
      cp ${pkgs.resume}/resume.pdf resume.pdf
      cp ${pkgs.resume}/resume.html resume.html

      # Create a file index for material/
      mkdir -p material
      pushd material
      tree -H '.' -L 1 \
        --noreport \
        --charset utf8 \
        -I index.html \
        -o index.html
      popd
      for dir in material/*/;
      do
        pushd $dir
        tree -H '.' -L 1 \
          --noreport \
          --charset utf8 \
          -I index.html \
          -o index.html
        popd
      done

      ${siteBuilder}/bin/site build
    '';

    installPhase = ''
      mkdir -p $out
      ${pkgs.rsync}/bin/rsync -rts _site/ $out
      touch $out/.nojekyll
    '';

    buildInputs = [ pkgs.tree pkgs.glibcLocales ];
  }
