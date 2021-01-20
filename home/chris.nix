{ config, lib, pkgs, ... }:

let 
  popupstatus = pkgs.callPackage ./scripts/popupstatus.nix { inherit config pkgs; };
  popupcommands = pkgs.callPackage ./scripts/popupcommands.nix { inherit config pkgs; };
  popupcommands_confirm = pkgs.callPackage ./scripts/popupcommands_confirm.nix { inherit config pkgs; };
  unzip_sjis = pkgs.callPackage ./scripts/unzip_sjis.nix { inherit config pkgs; };
  loadDesktopBackground = pkgs.callPackage ./scripts/loaddesktopbackground.nix { inherit config pkgs; };
  home-manager = pkgs.writeShellScriptBin "home-manager" ''
    # `toString` is required to impurely track your configuration instead of copying it to `/nix/store`
    exec ${pkgs.home-manager}/bin/home-manager -f ${toString ./home.nix} $@
  '';
in {
  imports = [
    ./programs/alacritty/default.nix
    ./programs/git/default.nix
    ./programs/xmonad/default.nix
    ./programs/rofi/default.nix
    /*./programs/nnn/default.nix*/
    ./services/dunst/default.nix
    ./services/gpg-agent/default.nix
    /*./services/load-background/default.nix*/
    ./programs/neovim/default.nix
    ./programs/autorandr/default.nix
    ./services/spotifyd/default.nix
    ../system/channels.nix
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
    convmv
    chromium 
    cudatoolkit
    curl
    dunst
    fd
    feh
    file
    firefox
    fzf
    gawk
    gcc
    gimp
    gnumake
    gparted
    home-manager
    jdk11
    jq
    killall
    kitty
    libnotify
    libreoffice
    loadDesktopBackground
    maim
    neovim
    neovim-remote
    nix-prefetch-git
    nnn
    nodejs
    p7zip
    pandoc
    pass
    pavucontrol
    pciutils
    nixpkgs-unstable.poetry
    popupstatus
    popupcommands
    popupcommands_confirm
    qutebrowser
    rofi
    rofi-pass
    sbt
    (scala.override { jre = pkgs.jdk11; })
    spotifyd
    nixpkgs-unstable.spotify-tui
    teams
    texlive.combined.scheme-full
    tree
    unzip
    unzip_sjis
    xcape
    xorg.xev
    zoom-us
  ];

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

  systemd.user.services.loadBackground = {
    Unit = {
      Description = "Set background image using feh";
      After = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.feh}/bin/feh --bg-scale /cfg/home/pictures/desktop_background.jpg";
      IOSchedulingClass = "idle";
    };
    Install = { WantedBy = [ "graphical-session.target" ]; };
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
