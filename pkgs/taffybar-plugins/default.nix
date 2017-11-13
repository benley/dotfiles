{ mkDerivation, base, bytestring, file-embed, gtk, stdenv, taffybar
, wirelesstools, yaml
}:
mkDerivation {
  pname = "taffybar-plugins";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring file-embed gtk taffybar yaml
  ];
  librarySystemDepends = [ wirelesstools ];
  homepage = "https://github.com/benley/dotfiles";
  license = stdenv.lib.licenses.bsd3;
}
