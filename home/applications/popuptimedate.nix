let
  popupTimeDateBattery = pkgs.writeShellScriptBin "popupTimeDateBattery" ''
    #!/bin/bash
    title="$(date +%H:%M)"
    date="$(date +%d) $(date +%B) $(date +%Y), $(date +%A)"
    battery=$( cat /sys/class/power_supply/BAT0/capacity )
    notify-send -i "$title" "$title $date Battery: $battery%"
  '';
in
stdenv.mkDerivation rec {
  name = "popupTimeDateBattery";
  buildInputs = [ popupTimeDateBattery pkgs.libnotify ];
}
