# Edit this configuration file to define what should be installed on # your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    # Channels
    ./system/channels.nix
    # home-manager
    (import "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/release-20.09.tar.gz}/nixos")
    # Xmonad
    ./system/wm/xmonad.nix
  ];

  hardware.bluetooth.enable = true;

  virtualisation.docker.enable = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  fileSystems."/data" = 
  { device = "/dev/disk/by-label/data";
    fsType = "ext4";
  };

  swapDevices=[
    { device="/var/swap";
      size=12288;}
  ];

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
  console.keyMap = "us";
  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "Asia/Tokyo";

  fonts.fonts = with pkgs; [
    roboto
    roboto-mono 
    dejavu_fonts
    ipafont
    kochi-substitute
  ];
  fonts.fontconfig.defaultFonts = {
    monospace = [
      "DejaVu Sans Mono"
      "IPAGothic"
    ];
    sansSerif = [
      "DejaVu Sans"
      "IPAPGothic"
    ];
    serif = [
      "DejaVu Serif"
      "IPAPMincho"
    ];
  };
  i18n.inputMethod.enabled = "fcitx";
  i18n.inputMethod.fcitx.engines = with pkgs.fcitx-engines; [ mozc ];

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };

  users.users.chris = {
    isNormalUser  = true;
    extraGroups   = [ "wheel" "networkmanager" "docker"]; # Enable ‘sudo’ for the user.
    shell         = pkgs.bash;
  };
  nix.trustedUsers = ["root" "chris" ];

  home-manager.users.chris = import ./home/chris.nix;

  environment.systemPackages = with pkgs; [
    bolt
    git
    gnome3.gnome-keyring
    libsecret
    networkmanager 
    openssh
    tlp
    udevil
    usbutils
    vim 
    wget 
    xclip
    xorg.xkbcomp 
    nixpkgs-unstable.xterm 
  ];

  # CUDA stuff
  nixpkgs.config.allowUnfree = true;
  #services.xserver.videoDrivers = [ "nvidia" ];
  #systemd.user.services.nvidia-control-devices = {
    #wantedBy = [ "multi-user.target" ];
    #serviceConfig.ExecStart = "${pkgs.linuxPackages.nvidia_x11.bin}/bin/nvidia-smi";
  #};

  services.tlp.enable = true;

  # For mailspring
  services.gnome3.gnome-keyring.enable = true;

  nix = {
    autoOptimiseStore = true;
    gc = {
      automatic = true;
      dates     = "weekly";
      options   = "--delete-older-than 7d";
    };
  };
  environment.variables.NIX_CONFIG = "/cfg/configuration.nix";

  # Binary Cache for Haskell.nix
  nix.binaryCachePublicKeys = [
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
  ];
  nix.binaryCaches = [
    "https://hydra.iohk.io"
  ];

  services.hardware.bolt.enable = true;
  environment.variables.EDITOR = "vim";
  
  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  programs.ssh.startAgent = true;
  programs.ssh.agentTimeout = null;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  system.stateVersion = "20.09"; # Did you read the comment?

}

