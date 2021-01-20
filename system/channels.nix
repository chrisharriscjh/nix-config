{ ...}:
let

  pkgsConfig = {
    allowUnfree = true;
  };

  packageSetsOverlay = self: super: {
    nixpkgs-unstable = import (
      fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz
    ) { config = pkgsConfig; };
    stable = import (
      fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz
    ) { config = pkgsConfig; };
  };

  overlays = [ packageSetsOverlay
             ];
in
{
  nixpkgs.overlays = overlays;
  nixpkgs.config = pkgsConfig;
}
