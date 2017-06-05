{ nixpkgs ? import <nixpkgs> {} }:

with { inherit (nixpkgs) pkgs lib; };

rec {
  inherit (pkgs) callPackage;

  kubernetes-client = callPackage ./pkgs/kubernetes-client { };

  nix-home = callPackage ./pkgs/nix-home { };

  nixhomeLib = callPackage (import "${nix-home}/nix/lib/nixhome") {};

  mkHome = nixhomeLib.mkHome;

  homedir = import ./homedir.nix { inherit lib mkHome; };

  myVim = pkgs.vim_configurable.customize {
    name = "vim";
    vimrcConfig = {
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
          # "vim-addon-nix"  # The MarcWeber version
          "vim-nix"  # The LnL7 one
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

  steamcontroller-udev-rules = callPackage ./pkgs/steamcontroller-udev-rules { };

  yaml2json = callPackage ./pkgs/yaml2json { };
}
