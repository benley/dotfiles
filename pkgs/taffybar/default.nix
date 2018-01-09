{ mkDerivation, base, cairo, ConfigFile, containers, dbus
, directory, dyre, either, enclosed-exceptions, filepath, glib
, gtk-traymanager, gtk3, HStringTemplate, HTTP, mtl, multimap
, network, network-uri, old-locale, parsec, process, rate-limit
, safe, split, stdenv, stm, text, time, time-locale-compat
, time-units, transformers, tuple, unix, utf8-string, X11
, xdg-basedir, xml, xml-helpers, xmonad, xmonad-contrib
, pkgs_gtk3, fetchFromGitHub
}:
mkDerivation {
  pname = "taffybar";
  version = "1.0.0-pre";
  src = fetchFromGitHub {
    owner = "benley";
    repo = "taffybar";
    rev = "6e481d257b3cb3d729113457160d19a29e371723";
    sha256 = "0hvjxh0n9csswddwnvax32j46kl49q28gk98ypnh3aaq815c3iyf";
  };
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryPkgconfigDepends = [ pkgs_gtk3 ];
  libraryHaskellDepends = [
    base cairo ConfigFile containers dbus directory dyre either
    enclosed-exceptions filepath glib gtk-traymanager gtk3
    HStringTemplate HTTP mtl multimap network network-uri old-locale
    parsec process rate-limit safe split stm text time
    time-locale-compat time-units transformers tuple unix utf8-string
    X11 xdg-basedir xml xml-helpers xmonad xmonad-contrib
  ];
  executableHaskellDepends = [
    base containers directory dyre filepath glib gtk3 mtl safe split
    utf8-string X11 xdg-basedir
  ];
  homepage = "http://github.com/travitch/taffybar";
  description = "A desktop bar similar to xmobar, but with more GUI";
  license = stdenv.lib.licenses.bsd3;
}
