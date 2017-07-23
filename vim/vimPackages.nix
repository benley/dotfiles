{ pkgs, lib, ... }:

let
  customization = {
    name = "vim";
    vimrcConfig = (import ./vim/customization.nix { inherit pkgs; });
  };

  custom_vim = pkgs.vim_configurable.customize customization;

  vim = lib.overrideDerivation custom_vim (o: {
    # ...
  });

in [
  vim
  pkgs.python
  pkgs.ctags
]
