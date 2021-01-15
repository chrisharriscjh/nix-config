#{ config, pkgs, lib, ... }:
#with lib; {

  #config.nixpkgs.config = {
    ## Allow proprietary packages
    #allowUnfree = true;

    ## Create an alias for the unstable channel
    #packageOverrides = pkgs: rec {

      #stable = import (fetchTarball
        #"https://nixos.org/channels/nixos-20.09/nixexprs.tar.xz") {
          #config = config.nixpkgs.config;
        #};
      #nixos-unstable = import (fetchTarball
        #"https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz") {
          ## pass the nixpkgs config to the unstable alias
          ## to ensure `allowUnfree = true;` is propagated:
          #config = config.nixpkgs.config;
        #};
      #nixpkgs-unstable = import (fetchTarball
        #"https://github.com/nixos/nixpkgs-channels/archive/nixpkgs-unstable.tar.gz") {
          ## pass the nixpkgs config to the unstable alias
          ## to ensure `allowUnfree = true;` is propagated:
          #config = config.nixpkgs.config;
        #};
    #};
  #};
#}
{ ...}:
let

  pkgsConfig = {
    allowUnfree = true;
  };

  packageSetsOverlay = self: super: {
    #stable = import (fetchTarball
      #"https://nixos.org/channels/nixos-20.09/nixexprs.tar.xz") {
        #config = config.nixpkgs.config;
      #};
    #nixos-unstable = import (fetchTarball
      #"https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz") {
        ## pass the nixpkgs config to the unstable alias
        ## to ensure `allowUnfree = true;` is propagated:
        #config = config.nixpkgs.config;
      #};
    #nixpkgs-unstable = import (fetchTarball
      #"https://github.com/nixos/nixpkgs-channels/archive/nixpkgs-unstable.tar.gz") {
        ## pass the nixpkgs config to the unstable alias
        ## to ensure `allowUnfree = true;` is propagated:
        #config = config.nixpkgs.config;
      #};
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
