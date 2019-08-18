{ pkgs ? import ./nix {} }:

let
  sources = import ./nix/sources.nix;
  siteBuilder = pkgs.haskellPackages.callPackage ./builder { inherit (pkgs) lib; };
  sourceByRegex = name: src: regexes:
    builtins.path
      { filter =  (path: type:
          let
            relPath = pkgs.lib.removePrefix (toString src + "/") (toString path);
            accept = pkgs.lib.any (re: builtins.match re relPath != null) regexes;
          in accept);
          inherit name;
          path = src;
      };
in
  pkgs.stdenv.mkDerivation rec {
    name = "nmattia-com-builder";

    src = sourceByRegex "nmattia-com" ./.
        [ "^.*.pdf$"
          "^boot$"
          "^images$"
          "^images/.*$"
          "^templates$"
          "^templates/.*$"
          "^material$"
          "^material/.*$"
          "^styles$"
          "^styles/default.css$"
          "^posts$"
          "^posts/.*$"
          "^[^R].*.md$" # allow all markdown files except README
        ];

    buildPhase = ''
      export LC_ALL=en_US.UTF-8
      export LANG=en_US.UTF-8
      export LANGUAGE=en_US.UTF-8
      cp ${pkgs.resume.pdf}/resume.pdf resume.pdf
      cp ${pkgs.resume.html}/resume.html resume.html

      cat ${sources.hakyll}/web/css/syntax.css >> ./styles/default.css

      mkdir -p icons
      unzip ${./favicon_package_v0.16.zip} -d icons

      color="fffff8"
      sed -i "s:XXX_COLOR:$color:g" ./templates/default.html

      # The site.webmanifest has some old color hard coded
      sed -i "s:#3b6484:#$color:g" ./icons/site.webmanifest

      hash=$(md5sum ./styles/default.css | cut -d ' ' -f 1)
      sed -i "s:XXX_CSS:$hash:g" ./templates/default.html

      hash=$(md5sum ./icons/apple-touch-icon.png | cut -d ' ' -f 1)
      sed -i "s:XXX_APPLE:$hash:g" ./templates/default.html

      hash=$(md5sum ./icons/favicon-32x32.png | cut -d ' ' -f 1)
      sed -i "s:XXX_FAV32:$hash:g" ./templates/default.html

      hash=$(md5sum ./icons/favicon-16x16.png | cut -d ' ' -f 1)
      sed -i "s:XXX_FAV16:$hash:g" ./templates/default.html

      hash=$(md5sum ./icons/site.webmanifest | cut -d ' ' -f 1)
      manifest_name="site.webmanifest.$hash"
      mv ./icons/site.webmanifest "./icons/$manifest_name"
      sed -i "s:site.webmanifest:$manifest_name:g" ./templates/default.html

      hash=$(md5sum ./icons/safari-pinned-tab.svg | cut -d ' ' -f 1)
      sed -i "s:XXX_SAFARI:$hash:g" ./templates/default.html

      cp -r ${sources.tufte-css}/et-book ./styles/fonts

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

    buildInputs = [ pkgs.tree pkgs.glibcLocales pkgs.unzip ];
  }
