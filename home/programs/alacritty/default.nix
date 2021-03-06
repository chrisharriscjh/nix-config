{ fontSize, pkgs, ... }:

{
  programs.alacritty = {
    enable = true;
    settings = {
      background_opacity = 1;
      bell = {
        animation = "EaseOutExpo";
        duration = 5;
        color = "#ffffff";
      };
      colors = {
        primary = {
          background = "#2A3439";
          foreground = "#c5c8c6";
        };
      };
      /*font = {*/
        /*normal = {*/
          /*family = "Roboto Mono";*/
        /*};*/
        /*[>size = fontSize;<]*/
      /*};*/
      selection.save_to_clipboard = true;
      /*shell.program = "${pkgs.fish}/bin/fish";*/
      window = {
        decorations = "full";
        padding = {
          x = 5;
          y = 5;
        };
      };
    };
  };
}
