{ mkDerivation, base, directory, filepath, gtk-traymanager, gtk3
, stdenv, taffybar, X11, xmonad-contrib
, wirelesstools, yaml, file-embed, bytestring, text, gi-gtk, process, safe
}:
mkDerivation {
  pname = "taffybar-plugins";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  libraryHaskellDepends = [
    base directory filepath gtk-traymanager gtk3 taffybar X11
    xmonad-contrib file-embed yaml bytestring text gi-gtk process
  ];
  librarySystemDepends = [ wirelesstools ];
  homepage = "https://github.com/benley/dotfiles";
  license = stdenv.lib.licenses.bsd3;
}
