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
    rev = "d3c7e2648d81687576a186b3959519aa36b0bb1d";
    sha256 = "12jkiqwsbx380fsiizbwva2607ih52kljfkap3icajxcds15ia5s";
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
