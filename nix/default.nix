{}:
let
  sources = import ./sources.nix;
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
  pkgs = import sources.nixpkgs extra;
in
pkgs //
  { resume = import sources.resume { inherit pkgs; }; } //
  { niv = (import sources.niv { inherit pkgs; }).niv; }
