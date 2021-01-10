{ config, pkgs, ... }:

let 
  laptopScreen = "00ffffffffffff004d10ad14000000002a1c0104a51d11780ede50a3544c99260f5054000000010101010101010101010101010101014dd000a0f0703e803020350026a510000018a4a600a0f0703e803020350026a510000018000000fe00305239394b804c513133334431000000000002410328011200000b010a20200041";
  homeGamingMonitor = "00ffffffffffff001e6dfe7664960500071c0104a55022789eca95a6554ea1260f5054256b80d1cfd1e8d1fca9fc81bc81fc617c717c644b00f0a03846403020aa001e4e3100001aec5e00f0a0384e403020aa001e4e3100001a000000fd0038901ea42d000a202020202020000000fc004c4720554c545241574944450a014a020316712309060749100403011f1359da12830100008c0ad08a20e02d10103e96001e4e31000018507800a0a03835403020ca001e4e3100001a139000a0a03832403020ca001e4e3100001ae4ac00a0a03832403020ca001e4e3100001a000000000000000000000000000000000000000000000000000000000000000000ea";
in {
  programs.autorandr = {
    enable = true;
    profiles = {
      "laptop" = {
        fingerprint = {
          "eDP-1" = laptopScreen;
        };
        config = {
          "eDP-1" = {
            enable = true;
            primary = true;
            position = "0x0";
            mode = "1920x1080";
            #gamma = "1.0:0.909:0.833";
            #rate = "60.00";
          };
        };
      };

      "home" = {
        fingerprint = {
          "eDP-1" = laptopScreen;
          "DP-2" = homeGamingMonitor;
        };
        config = {
          "eDP-1" = {
            enable = true;
            primary = true;
            position = "0x0";
            mode = "1920x1080";
            #gamma = "1.0:0.909:0.833";
            #rate = "60.00";
          };

          "DP-2" = {
            enable = true;
            primary = false;
            position = "1920x0";
            mode = "2560x1080";
            #gamma = "1.0:0.909:0.833";
            #rate = "60.00";
          };
        };
      };
    };
  };
}
