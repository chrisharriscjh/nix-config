{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.services.loadBackground;

in {

  options = {
    services.loadBackground = {
      default = true;
      enable = mkEnableOption "loadBackground";
    };
  };

  config = mkIf cfg.enable {
    systemd.user.services.loadBackground = {
      Unit = {
        Description = "Set background image using feh";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Service = {
        Type = "oneshot";
        ExecStart = "${pkgs}.loadDesktopBackground";
        IOSchedulingClass = "idle";
      };

      Install = { WantedBy = [ "graphical-session.target" ]; };
    };
  };
}
