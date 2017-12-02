# Based on stuff from:
#  https://beyermatthias.de/blog/2015/11/25/how-to-setup-neovim-on-nixos/
#  http://nerditya.com/code/guide-to-neovim/

{ pkgs, lib, ... }:

let
  custom_vim = pkgs.vim_configurable.customize {
    name = "vim";
    vimrcConfig = (import ./customization.nix { inherit pkgs; });
  };

in {
  vim = lib.overrideDerivation custom_vim (o: {
    ftNixSupport = false;
  });

  neovim = pkgs.neovim.override {
    vimAlias = true;

    configure = (import ./customization.nix { inherit pkgs; });
  };
}
