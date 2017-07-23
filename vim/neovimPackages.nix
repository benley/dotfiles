{ pkgs, lib, ... }:

let nvim = pkgs.neovim.override {
  vimAlias = false;

  configure = (import ./customization.nix { inherit pkgs; });
}; in [
  nvim
  pkgs.python
  pkgs.ctags
]
