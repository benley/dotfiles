{ nixpkgs ? import <nixpkgs> {} }:

with { inherit (nixpkgs) pkgs lib; };

rec {
  inherit (pkgs) callPackage;

  nix-home = callPackage ./pkgs/nix-home { };

  nixhomeLib = callPackage (import "${nix-home}/nix/lib/nixhome") {};

  mkHome = nixhomeLib.mkHome;

  homedir = import ./homedir.nix { inherit lib mkHome; };

  myVim = pkgs.vim_configurable.customize {
    name = "vim";
    vimrcConfig = {
      #customRC = builtins.readFile ./vimrc;
      customRC = ''
        source ~/.vimrc
        set secure
      '';
      vam.knownPlugins = pkgs.vimPlugins;
      vam.pluginDictionaries = [{
        names = [
          "Solarized"
          "Supertab"
          "Syntastic"
          "Tagbar"
          "The_NERD_tree"
          "fugitive"
          "ghcmod"
          "molokai"
          "neco-ghc"
          "rainbow_parentheses"
          "rust-vim"
          "tabular"
          "taglist"
          "vim-addon-nix"
          "vim-airline"
          "vim-airline-themes"
          "vim-buffergator"
          "vim-coffee-script"
          "vim-eunuch"
          "vim-gitgutter"
          "vim-go"
          "vim-jsonnet"
          "vim2hs"
          "vimproc"
          "vimshell-vim"
          "youcompleteme"
          # "vim-hdevtools"
          "zenburn"
        ];
      }];
    };
  };

}
