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
    ./programs/git/default.nix
    ./programs/xmonad/default.nix
    ./programs/rofi/default.nix
  ];

  nixpkgs.config.allowUnfree = true;

  home.sessionVariables = {
    EDITOR = "nvim";
    TEST_THEN_DELETE_ME = "working";
  };
  home.packages = with pkgs; [ 
    any-nix-shell
    alacritty
    autorandr
    conda
    chromium 
    curl
    dunst
    fd
    feh
    firefox
    fzf
    gimp
    gparted
    jdk11
    killall
    kitty
    libnotify
    libreoffice
    maim
    neovim
    neovim-remote
    nnn
    nodejs
    pavucontrol
    poetry
    popupTimeDate
    qutebrowser
    rofi
    sbt
    (scala.override { jre = pkgs.jdk11; })
    teams
    tree
    xcape
    xorg.xev
    zoom-us
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
  services.xcape = {
    enable = true;
    mapExpression = { Caps_Lock = "Escape"; };
  };
}
