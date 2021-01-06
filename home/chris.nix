{ config, lib, pkgs, stdenv, ... }:

let
  popupTimeDate = pkgs.writeScriptBin "popupTimeDate" ''
    title="$(date +%H:%M)"
    date="$(date +%d) $(date +%B) $(date +%Y), $(date +%A)"
    battery=$( cat /sys/class/power_supply/BAT0/capacity )
    notify-send -i "$title" "$title $date Battery: $battery%"
  '';
in {
  imports = [
    ./programs/xmonad/default.nix
  ];

  nixpkgs.config.allowUnfree = true;
  programs.git = {
    enable = true;
    userName = "chrisharriscjh";
    userEmail = "chrisharris_cjh@hotmail.com";
  };
  home.sessionVariables = {
    EDITOR = "nvim";
    TEST_THEN_DELETE_ME = "working";
  };
  home.packages = [ 
    pkgs.alacritty
    popupTimeDate
    pkgs.autorandr
    pkgs.chromium 
    pkgs.curl
    pkgs.dunst
    pkgs.libnotify
    pkgs.fzf
    pkgs.neovim
    pkgs.nnn
    pkgs.qutebrowser
    pkgs.firefox
    pkgs.xcape
    pkgs.feh
    pkgs.rofi
    pkgs.neovim-remote
    pkgs.pavucontrol
    pkgs.gparted
    pkgs.fd
    pkgs.zoom-us
    pkgs.tree
    pkgs.xorg.xev
    pkgs.python3
    pkgs.pythonPackages.pip
    pkgs.conda
  ];
  home.file.".vimrc".source = /cfg/vimrc;
  home.username = "chris";
  home.homeDirectory = "/home/chris";
  home.stateVersion = "21.03";
  services.dunst = {
    enable = true;
    settings = {
      global = {
        transparency = 10;
        notification_height = 0;
        separator_height = 2;
        padding = 15;
        geometry = "600x50-50+65";
        follow = "keyboard";
        markup = "full";
        font = "Roboto";
        format = ''<b>%s</b>\n%b'';
      };
    };
  #xsession.windowManager.i3.config.startup = [{ command = "${pkgs.dunst}/bin/dunst"; }];
  };
  services.screen-locker = {
    enable = true;
    inactiveInterval = 10;
    lockCmd = "${pkgs.betterlockscreen}/bin/betterlockscreen -l dim";
    xautolockExtraOptions = [
      "Xautolock.killer: systemctl suspend"
    ];
  };
  services.picom = {
    enable = true;
    activeOpacity = "1.0";
    inactiveOpacity = "0.8";
    backend = "glx";
    fade = true;
    fadeDelta = 5;
    opacityRule = [ "100:name *= 'i3lock'" ];
    shadow = true;
    shadowOpacity = "0.75";
  };
}
