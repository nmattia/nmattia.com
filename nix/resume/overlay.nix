self: super: { resume = import (self.fetch ./src.json) { pkgs = self; }; }
