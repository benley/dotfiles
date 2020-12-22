{ mkDerivation, base, stdenv, xmonad, xmonad-contrib }:
mkDerivation {
  pname = "benley-xmonad-config";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base xmonad xmonad-contrib ];
  description = "benley's xmonad config";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
