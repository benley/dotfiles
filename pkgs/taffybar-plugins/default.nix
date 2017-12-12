{ mkDerivation, base, bytestring, file-embed, gtk3, stdenv, taffybar
, wirelesstools, yaml
}:
mkDerivation {
  pname = "taffybar-plugins";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring file-embed gtk3 taffybar yaml
  ];
  librarySystemDepends = [ wirelesstools ];
  homepage = "https://github.com/benley/dotfiles";
  license = stdenv.lib.licenses.bsd3;
}
