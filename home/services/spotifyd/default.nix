{ pkgs, ... }:

{
  services.spotifyd = {
    enable = true;
    settings = {
      global = {
        username = "chrisharris_cjh@hotmail.com";
        /*username = "uubeflt8x49nrel8dm542lr29";*/
        /*password_cmd = "${pkgs.pass}/bin/pass spotify | ${pkgs.gawk}/bin/gawk NR==1";*/
        password = "ExamplePassword";
        device_name = "nix";
        /*backend = "pulseaudio";*/
        backend = "alsa";
      };
    };
  };
}

