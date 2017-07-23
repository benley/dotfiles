{ pkgs }:

let
  vimrc = pkgs.callPackage ./vimrc.nix {};
  plugins = pkgs.callPackage ./plugins.nix {};
in {
  customRC = vimrc;
  vam = {
    knownPlugins = pkgs.vimPlugins // plugins;

    pluginDictionaries = [
      { name = "youcompleteme"; }
      { name = "vim-trailing-whitespace"; }
      { name = "Solarized"; }
      { name = "Supertab"; }
      { name = "Syntastic"; }
      { name = "Tagbar"; }
      { name = "The_NERD_tree"; }
      { name = "fugitive"; }
      { name = "ghcmod"; }
      { name = "molokai"; }
      { name = "neco-ghc"; }
      { name = "rainbow_parentheses"; }
      { name = "rust-vim"; }
      { name = "tabular"; }
      { name = "taglist"; }
      # { name = "vim-addon-nix"; } # The MarcWeber version
      { name = "vim-nix"; }  # The LnL7 one
      { name = "vim-airline"; }
      { name = "vim-airline-themes"; }
      { name = "vim-buffergator"; }
      { name = "vim-coffee-script"; }
      { name = "vim-eunuch"; }
      { name = "vim-gitgutter"; }
      { name = "vim-go"; }
      { name = "vim-jsonnet"; }
      { name = "vim2hs"; }
      { name = "vimproc"; }
      { name = "vimshell-vim"; }
      { name = "youcompleteme"; }
      # { name = "vim-hdevtools"; }
      { name = "zenburn"; }
    ];
  };
}
