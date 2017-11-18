{ mkDerivation, base, cairo, ConfigFile, containers, dbus
, directory, dyre, either, enclosed-exceptions, fetchFromGitHub, filepath
, gtk, gtk-traymanager, gtk2, HStringTemplate, HTTP, mtl, multimap
, network, network-uri, old-locale, parsec, process, rate-limit
, safe, split, stdenv, stm, text, time, time-locale-compat
, time-units, transformers, tuple, unix, utf8-string, X11
, xdg-basedir, xml, xml-helpers, xmonad, xmonad-contrib
}:

mkDerivation {
  pname = "taffybar";
  version = "0.4.6-git-pr242";
  src = fetchFromGitHub {
    owner = "benley";
    repo = "taffybar";
    rev = "ebe8f864dd1e4310e95993ac5f4f0c1c0c5a01a2";
    sha256 = "0nj8lb953abkyqvfr8zw3h9fs2x6zwzn0wirfv16yx4012y1k3xv";
  };
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base cairo ConfigFile containers dbus directory dyre either
    enclosed-exceptions filepath gtk gtk-traymanager HStringTemplate
    HTTP mtl multimap network network-uri old-locale parsec process
    rate-limit safe split stm text time time-locale-compat time-units
    transformers tuple unix utf8-string X11 xdg-basedir xml xml-helpers
    xmonad xmonad-contrib
  ];
  libraryPkgconfigDepends = [ gtk2 ];
  executableHaskellDepends = [
    base containers dyre filepath gtk mtl safe split utf8-string X11
    xdg-basedir
  ];
  executablePkgconfigDepends = [ gtk2 ];
  homepage = "http://github.com/travitch/taffybar";
  description = "A desktop bar similar to xmobar, but with more GUI";
  license = stdenv.lib.licenses.bsd3;
}
