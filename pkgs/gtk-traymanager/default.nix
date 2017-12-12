{ mkDerivation, base, fetchgit, glib, gtk3, stdenv, x11, pkgs_gtk3 }:
mkDerivation {
  pname = "gtk-traymanager";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/IvanMalison/gtk-traymanager.git";
    sha256 = "1ryfb2lhyyaynjxjzvjafwnvf6sww4abicjjp6n94cjp7djsd100";
    rev = "517b24722b2cdab2dadbafa882ffcb6b8a1d01ff";
  };
  libraryHaskellDepends = [ base glib gtk3 ];
  libraryPkgconfigDepends = [ x11 gtk3 pkgs_gtk3 ];
  homepage = "http://github.com/travitch/gtk-traymanager";
  description = "A wrapper around the eggtraymanager library for Linux system trays";
  license = stdenv.lib.licenses.lgpl21;
}
