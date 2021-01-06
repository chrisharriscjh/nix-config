{ config, lib, pkgs, ... }:

let modMapAdjustments = pkgs.writeText "xkb-layout" ''
  keycode 102 = Super_L
  '';
in
{
  services.xserver = {
    enable = true;
    libinput.enable = true;

    #extraLayouts.us-custom = {
    #  description = "US layout with custom hyper keys";
    #  languages   = [ "eng" ];
    #  symbolsFile = ./us-custom.xkb;
    #};

    displayManager.defaultSession = "none+xmonad";
    displayManager.sessionCommands = "${pkgs.xorg.xmodmap}/bin/xmodmap ${modMapAdjustments}";

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };

    xkbOptions = "altwin:swap_alt_win,caps:ctrl_modifier";
  };
}
