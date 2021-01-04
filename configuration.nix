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
  networking.wireless.networks = {
    "Free the Paedos 5G" = {
      psk = "55328478";
    };
    "FabbitAoyama" = {
      psk = "Fabbit6005";
    };
  };

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  
  time.timeZone = "Asia/Tokyo";

  #console = {
    #font = "Roboto";
    #keyMap = "us";
  #};

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

  home-manager.users.chris = { 
    programs.git = {
      enable = true;
      userName = "chrisharriscjh";
      userEmail = "chrisharriscjh@gmail.com";
    };
    home.sessionVariables = {
      EDITOR = "nvim";
      TEST_THEN_DELETE_ME = "working";
    };
    home.packages = [ 
      pkgs.dunst
      pkgs.curl
      pkgs.nnn
      pkgs.neovim
      pkgs.qutebrowser
      pkgs.autorandr
      pkgs.firefox
      pkgs.chromium 
      pkgs.alacritty
      pkgs.fzf
      pkgs.xcape
      pkgs.feh
      pkgs.rofi
      pkgs.neovim-remote
      pkgs.pavucontrol
      pkgs.gparted
      pkgs.fd
    ];
    home.file.".vimrc".source = /cfg/vimrc;
    home.username = "chris";
    home.homeDirectory = "/home/chris";
    home.stateVersion = "21.03";
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget vim networkmanager git xorg.xkbcomp roboto-mono roboto xterm 
  ];

  #programs.neovim = {
  #  enable = true;
  #  configure = pkgs.neovimConfigure // {
  #    customRC = ''
  #      source /cfg/.vimrc
  #    '';
  #  };
  #};
  environment.variables.EDITOR = "nvim";
  #vimrc = import ./vimrc;
  #environment.etc."vimrc".text = vimrc;
  
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  system.stateVersion = "20.09"; # Did you read the comment?

  #home-manager.users.chris = {
    #services.dunst = {
      #enable = true;
      #settings = {
        #global = {
          #transparency = 16;
          #notification_height = 0;
          #separator_height = 2;
          #padding = 15;
          #follow = "keyboard";
          #markup = "full";
          #font = "Roboto";
          #format = "<b>%s</b>\n%b";
        #};
      #};
    ##xsession.windowManager.i3.config.startup = [{ command = "${pkgs.dunst}/bin/dunst"; }];
    #};
  #};
}

