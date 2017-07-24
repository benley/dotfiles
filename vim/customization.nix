{ pkgs }:

let
  vimrc = pkgs.callPackage ./vimrc.nix {};
  plugins = pkgs.callPackage ./plugins.nix {};
in {
  customRC = vimrc;
  vam = {
    knownPlugins = pkgs.vimPlugins // plugins;

    pluginDictionaries = [
      { name = "fugitive"; }
      { name = "ghcmod"; }
      { name = "molokai"; }
      { name = "neco-ghc"; }
      { name = "rainbow_parentheses"; }
      { name = "rust-vim"; }
      { name = "Solarized"; }
      { name = "Supertab"; }
      { name = "Syntastic"; }
      { name = "tabular"; }
      { name = "Tagbar"; }
      { name = "taglist"; }
      { name = "The_NERD_tree"; }
      # { name = "vim-addon-nix"; } # The MarcWeber version
      { name = "vim-airline"; }
      { name = "vim-airline-themes"; }
      { name = "vim-buffergator"; }
      { name = "vim-coffee-script"; }
      { name = "vim-eunuch"; }
      { name = "vim-gitgutter"; }
      { name = "vim-go"; }
      # { name = "vim-hdevtools"; }
      { name = "vim-jsonnet"; }
      { name = "vim-nix"; }  # The LnL7 one
      { name = "vim-trailing-whitespace"; }
      { name = "vim2hs"; }
      { name = "vimproc"; }
      { name = "vimshell-vim"; }
      # YCM triggers a bug in neovim:
      # https://github.com/neovim/neovim/issues/6166
      # { name = "youcompleteme"; }
      { name = "zenburn"; }
    ];
  };
}
