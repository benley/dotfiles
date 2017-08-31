{ pkgs }:

let
  vimrc = builtins.readFile ../cfg/.vimrc;
  plugins = pkgs.callPackage ./plugins.nix {};
in {
  customRC = vimrc;
  vam = {
    knownPlugins = pkgs.vimPlugins // plugins;

    pluginDictionaries = [
      # { name = "taglist"; }           # works fine, but I use Tagbar
      # { name = "vim-addon-nix"; }     # The MarcWeber version
      # { name = "vim-coffee-script"; } # Works fine, I just don't need it
      # { name = "vim-hdevtools"; }     # iirc the build broke or something?
      # { name = "vimshell-vim"; }      # I don't use this
      # { name = "youcompleteme"; }     # https://github.com/neovim/neovim/issues/6166
      { name = "base16-vim"; }
      { name = "dockerfile.vim"; }
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
      { name = "The_NERD_tree"; }
      { name = "vim-airline"; }
      { name = "vim-airline-themes"; }
      { name = "vim-buffergator"; }
      { name = "vim-eunuch"; }
      { name = "vim-gitgutter"; }
      { name = "vim-go"; }
      { name = "vim-jsonnet"; }
      { name = "vim-nix"; }  # The LnL7 one
      { name = "vim-terraform"; }
      { name = "vim-trailing-whitespace"; }
      { name = "vim-virtualenv"; }
      { name = "vim2hs"; }
      { name = "vimproc"; }
      { name = "zenburn"; }
    ];
  };
}
