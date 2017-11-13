{ mkDerivation, base, cairo, ConfigFile, containers, dbus
, directory, dyre, either, enclosed-exceptions, fetchgit, filepath
, gtk, gtk-traymanager, gtk2, HStringTemplate, HTTP, mtl, multimap
, network, network-uri, old-locale, parsec, process, rate-limit
, safe, split, stdenv, stm, text, time, time-locale-compat
, time-units, transformers, tuple, unix, utf8-string, X11
, xdg-basedir, xml, xml-helpers, xmonad, xmonad-contrib
}:

mkDerivation {
  pname = "taffybar";
  version = "0.4.6-git-2017-10-26";
  src = fetchgit {
    url = "https://github.com/travitch/taffybar.git";
    sha256 = "0r9hip4kf3bvgjqiffqxrkr8klldhpbhibj8kmmc38a8f271b08h";
    rev = "2647bc9071fb926d460e447ba1c333c06736b7d7";
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
