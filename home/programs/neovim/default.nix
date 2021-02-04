{ config, pkgs, ... }:

let cocSettings   = builtins.toJSON (import ./coc-settings.nix);
in {
  home.file.".vimrc".source = ./vimrc;
  xdg.configFile = {
    "nvim/coc-settings.json".text = cocSettings;
  };
}
