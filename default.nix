{ nixpkgs ? import <nixpkgs> {} }:

with { inherit (nixpkgs) pkgs lib; };

rec {
  inherit (pkgs) callPackage;

  nix-home = callPackage ./pkgs/nix-home { };

  nixhomeLib = callPackage (import "${nix-home}/nix/lib/nixhome") {};

  inherit (nixhomeLib) mkHome;

  homedir =
    let
      dotfiles = lib.cleanSource ./.;
      quickFiles = fs:
        lib.listToAttrs (map (f: lib.nameValuePair f "${dotfiles}/${f}") fs);

    in mkHome {
      user = "benley";
      files = {
        ".config/awesome/debian".link = "/etc/xdg/awesome/debian";
        ".irssi/" = "${dotfiles}/irssi";
        ".ssh/" = "${dotfiles}/ssh";
      } // quickFiles [
        ".Xresources"
        ".bash_completion.d/"
        ".bash_logout"
        ".bash_profile"
        ".bashrc"
        ".bashrc.d/"
        ".config/"
        ".ctags"
        ".devscripts"
        ".dircolors"
        ".emacs.d/"
        ".ghc/"
        ".gitconfig"
        ".githooks/"
        ".gnupg/"
        ".i3status.conf"
        ".inputrc"
        ".irbrc"
        ".nixpkgs/"
        ".pbuilderrc"
        ".pythonrc.py"
        ".quiltrc"
        ".screenrc"
        ".stack/"
        ".tmux.conf"
        ".vim/"
        ".vimrc"
        ".xmobarrc"
        ".xmonad/"
        ".xsettingsd"
      ];

    };
}
