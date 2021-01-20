with (import <unstable> {});
stdenv.mkDerivation {
  name = "unzip-iconv";
  builder = ./builder.sh; 
  src = fetchFromGitHub {
    owner = "m13253";
    repo = "unzip-iconv";
    url = "http://downloads.sourceforge.net/infozip/unzip60.tar.gz";
    sha256 = "0v1w00j1z5fj9ijbqnfavnvaym39xalnzqcgy7g46xj13mrikf58";
  };
  system = builtins.currentSystem;
}
