{ config, lib, pkgs, ... }:

let 
  popupstatus = pkgs.callPackage ./scripts/popupstatus.nix { inherit config pkgs; };
  popupcommands = pkgs.callPackage ./scripts/popupcommands.nix { inherit config pkgs; };
  popupcommands_confirm = pkgs.callPackage ./scripts/popupcommands_confirm.nix { inherit config pkgs; };
  unzip_sjis = pkgs.callPackage ./scripts/unzip_sjis.nix { inherit config pkgs; };
  loadDesktopBackground = pkgs.callPackage ./scripts/loaddesktopbackground.nix { inherit config pkgs; };
  #setJupyterConfig = pkgs.callPackage ./programs/jupyter/default.nix { inherit config pkgs; };
  home-manager = pkgs.writeShellScriptBin "home-manager" ''
    # `toString` is required to impurely track your configuration instead of copying it to `/nix/store`
    exec ${pkgs.home-manager}/bin/home-manager -f ${toString ./home.nix} $@
  '';
  julia_pkgs = import (builtins.fetchGit {
      # Descriptive name to make the store path easier to identify                
      name = "my-old-revision";                                                 
      url = "https://github.com/NixOS/nixpkgs/";                       
      ref = "refs/heads/nixpkgs-unstable";                     
      rev = "bed08131cd29a85f19716d9351940bdc34834492";                                           
  }) {};                                                                           

  julia153 = julia_pkgs.julia;
in {
  imports = [
    ./programs/alacritty/default.nix
    ./programs/git/default.nix
    ./programs/jupyter/default.nix
    ./programs/xmonad/default.nix
    ./programs/rofi/default.nix
    #./programs/mailspring/default.nix
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
    cudatoolkit
    curl
    docker-compose
    gitAndTools.diff-so-fancy
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
    ncurses
    nixpkgs-unstable.haskellPackages.haskell-language-server
    home-manager
    htop
    jdk11
    jq
    julia153
    #jupyterlab
    killall
    kitty
    libnotify
    libreoffice
    loadDesktopBackground
    mailspring
    maim
    ncpamixer
    neovim
    neovim-remote
    niv
    nix-prefetch-git
    nnn
    nodejs
    openvpn
    p7zip
    paperkey
    pandoc
    pass
    pciutils
    popupstatus
    popupcommands
    popupcommands_confirm
    qutebrowser
    rnix-lsp
    rofi
    rofi-pass
    sbt
    (scala.override { jre = pkgs.jdk11; })
    signal-desktop
    spotifyd
    nixpkgs-unstable.spotify-tui
    teams
    texlive.combined.scheme-full
    tree
    unetbootin
    unzip
    unzip_sjis
    vlc
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
    bash = {
      enable = true;
      historyFileSize = 1000000;
      historyControl = [ "erasedups" "ignoredups" ];
      historyIgnore = [ "ls" "cd" "exit" ];
      initExtra = ''
        [ -f ~/.fzf.bash ] && source ~/.fzf.bash
      eval "$(direnv hook bash)"
      eval "$(ssh-agent -s)" > /dev/null
      # For neovim remote auto-completion
      _nvr_opts_completions()
      {
          local cur prev opts
          cur=''\${COMP_WORDS[COMP_CWORD]}
          prev=''\${COMP_WORDS[COMP_CWORD-1]}
          opts=(
              -h
              -cc
              -c
              -d
              -l
              -o
              -O
              -p
              -q
              -s
              -t
              --nostart
              --version
              --serverlist
              --servername
              --remote
              --remote-wait
              --remote-silent
              --remote-wait-silent
              --remote-tab
              --remote-tab-wait
              --remote-tab-silent
              --remote-tab-wait-silent
              --remote-send
              --remote-expr
          )
          case "''\${prev}" in
              --servername)
                  srvlist=$(nvr --serverlist)
                  COMPREPLY=( $(compgen -W "''\${srvlist}" -- "$cur") )
                  return 0
                  ;;
          esac
          if [[ "$cur" =~ ^- ]]; then
              COMPREPLY=( $(compgen -W "''\${opts[*]}" -- "$cur") )
              return 0
          fi

          COMPREPLY=()
          return 0
      }

      complete -o default -F _nvr_opts_completions nvr
      '';
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
    enable = false;
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
