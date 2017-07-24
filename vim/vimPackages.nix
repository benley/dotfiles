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
