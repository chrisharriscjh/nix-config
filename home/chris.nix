{ config, lib, pkgs, ... }:

let
  popupstatus = pkgs.callPackage ./scripts/popupstatus.nix { inherit config pkgs; };
  popupcommands = pkgs.callPackage ./scripts/popupcommands.nix { inherit config pkgs; };
  popupcommands_confirm = pkgs.callPackage ./scripts/popupcommands_confirm.nix { inherit config pkgs; };
in {
  imports = [
    ./programs/git/default.nix
    ./programs/xmonad/default.nix
    ./programs/rofi/default.nix
    ./services/dunst/default.nix
    ./services/gpg-agent/default.nix
  ];

  nixpkgs.config.allowUnfree = true;

  home.sessionVariables = {
    EDITOR = "nvim";
  };
  home.packages = with pkgs; [ 
    any-nix-shell
    alacritty
    autorandr
    betterlockscreen
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
    jq
    killall
    kitty
    libnotify
    libreoffice
    maim
    neovim
    neovim-remote
    nnn
    nodejs
    pass
    pavucontrol
    poetry
    popupstatus
    popupcommands
    popupcommands_confirm
    qutebrowser
    rofi
    rofi-pass
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

  programs = {
    gpg.enable = true;
    password-store.enable = true;
    password-store.package = pkgs.pass;
    password-store.settings = {
      PASSWORD_STORE_DIR = "/cfg/home/pass";
      PASSWORD_STORE_KEY = "chrisharriscjh@gmail.com";
    };
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
