# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  nix.nixPath = [
    "nixos-config=/cfg" 
    "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos/nixpkgs"
  ];

  imports = [
    ./hardware-configuration.nix
    (import "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/release-20.09.tar.gz}/nixos")
  ];

  hardware.bluetooth.enable = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  fileSystems."/data" = 
  { device = "/dev/disk/by-label/data";
    fsType = "ext4";
  };

  fileSystems."/arch" = 
  { device = "/dev/disk/by-label/arch";
    fsType = "ext4";
  };


  networking.hostName = "nixos"; # Define your hostname.
  #networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.networkmanager.enable = true;
  networking.useDHCP = false;
  networking.interfaces.wlp1s0.useDHCP = true;
  networking.wireless.networks = import ./wifi-networks.nix;
  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  
  time.timeZone = "Asia/Tokyo";

  services.xserver = { 
    windowManager.xmonad = { 
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
	haskellPackages.xmonad-extras
	haskellPackages.xmonad
      ];
    };
    enable = true;
    displayManager.defaultSession = "none+xmonad";
    desktopManager.plasma5.enable = true;
  };
  

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };

  users.users.chris = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ]; # Enable ‘sudo’ for the user.
  };

  nixpkgs.config.allowUnfree = true;
  home-manager.users.chris = { 
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
      #import ./applications/popuptimedate.nix
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
  };

  environment.systemPackages = with pkgs; [
    wget vim networkmanager git xorg.xkbcomp roboto-mono roboto xterm xclip
  ];

  environment.variables.EDITOR = "vim";
  
  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  system.stateVersion = "20.09"; # Did you read the comment?

}

