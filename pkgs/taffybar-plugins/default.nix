{ mkDerivation, base, gtk, stdenv, taffybar, wirelesstools }:

mkDerivation {
  pname = "taffybar-plugins";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base gtk taffybar ];
  librarySystemDepends = [ wirelesstools ];
  executableHaskellDepends = [ base gtk taffybar ];
  testHaskellDepends = [ base ];
  homepage = https://github.com/benley/dotfiles;
  license = stdenv.lib.licenses.bsd3;
}
