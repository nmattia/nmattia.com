{}:
let
  sources = import ./sources.nix;
  pkgs = sources.nixpkgs;
  fetchOverlay = self: super:
    { fetch = path:
        let
          dropBranch = {...}@attrs:
            { inherit (attrs) rev sha256 owner repo; };
          fullSpec = (builtins.fromJSON (builtins.readFile path));
        in self.fetchFromGitHub (dropBranch fullSpec);
    };

  extra =
    { overlays =
        [
          fetchOverlay
          (self: super: { resume = import sources.resume { pkgs = self; }; })
        ];
    };
in import pkgs extra
