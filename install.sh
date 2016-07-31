#!/usr/bin/env bash

#TODO(benley): --dryrun isn't actually implemented.

set -o errexit

OS=$(uname)

die() {
  LogError "$@"
  exit 1
}

LogError() {
  [[ $# -lt 1 ]] && LogError "LogError: no error text given"
  echo "ERROR: $(basename "$0"): $*" 1>&2
}

PrintUsage() {
#FIXME(benley): implement this
:
}

GetFileInode() {
  case "$OS" in
    "Darwin")  stat -L -f%i "$1" 2>/dev/null        ;;
     "Linux")  stat -L -c%i "$1" 2>/dev/null        ;;
           *)  die "Unknown OS: '$OS'; giving up."  ;;
  esac
}

SameFile() {
  # Return 0 if $1 and $2 are hard or soft links to the same file.
  # (That is, if they point at the same inode, or are literally the same file.)
  [[ $(GetFileInode "$1") -eq $(GetFileInode "$2") ]] && return 0
  return 1
}

Symlink() {
  # Symlink $1 to $2.
  # If force=1, don't confirm before overwriting existing files.
  if ((FLAGS_force)); then
    ln -sfn "$1" "$2" || true
  else
    ln -sin "$1" "$2" || true
  fi
}

SymlinkIfDiffer() {
  # Unless $1 and $2 are the same file or symlinks to the same file, symlink $1
  # to $2.
  if ! SameFile "$1" "$2"; then
    Symlink "$1" "$2"
  fi
}

main() {
  export FLAGS_force=0
  export FLAGS_dryrun=0
  #TODO(benley): is export necessary here?

  for arg in "$@"; do
    case "$arg" in
      "-f" | "--force")
        readonly FLAGS_force=1
      ;;
      "--dryrun")
        readonly FLAGS_dryrun=0
      ;;
      "-h"|"-?"|"--help")
        PrintUsage
        exit 0
        ;;
      *)
        LogError "Unrecognized option: $arg"
        PrintUsage >&2
        exit 1
      ;;
    esac
  done

  readonly rootdir=$(cd "$(dirname "$0")"; pwd)

  set -o nounset
  dotfiles=(.inputrc .bash_logout .bash_profile .bashrc .dircolors
            .pythonrc.py .screenrc .tmux.conf .quiltrc .Xresources .irbrc
            .gitconfig .ctags .devscripts .xmobarrc .pbuilderrc .xsettingsd
            .i3status.conf)
  for file in "${dotfiles[@]}"; do
    SymlinkIfDiffer "$rootdir/$file" "$HOME/$file"
  done

  mkdir -p "$HOME/.bash_completion.d"
  for file in $rootdir/.bash_completion.d/*; do
    SymlinkIfDiffer "$file" "$HOME/.bash_completion.d/$(basename "$file")"
  done

  mkdir -p "$HOME/.bashrc.d"
  for file in $rootdir/.bashrc.d/*; do
    SymlinkIfDiffer "$file" "$HOME/.bashrc.d/$(basename "$file")"
  done

  mkdir -p "$HOME/.xmonad"
  for file in $rootdir/.xmonad/*; do
    SymlinkIfDiffer "$file" "$HOME/.xmonad/$(basename "$file")"
  done

  mkdir -p "$HOME/.ghc"
  SymlinkIfDiffer "$rootdir/.ghc/ghci.conf" "$HOME/.ghc/ghci.conf"

  mkdir -p "$HOME/.ssh"
  SymlinkIfDiffer "$rootdir/ssh/config" "$HOME/.ssh/config"
  SymlinkIfDiffer "$rootdir/ssh/rc" "$HOME/.ssh/rc"
  SymlinkIfDiffer "$rootdir/.vimrc" "$HOME/.vimrc"
  SymlinkIfDiffer "$rootdir/.vim" "$HOME/.vim"
  SymlinkIfDiffer "$rootdir/.githooks" "$HOME/.githooks"
  SymlinkIfDiffer "$rootdir/.config/awesome" "$HOME/.config/awesome"
  SymlinkIfDiffer "$rootdir/.config/i3" "$HOME/.config/i3"
  SymlinkIfDiffer "$rootdir/.nixpkgs" "$HOME/.nixpkgs"

  # for virtualenvwrapper
  mkdir -p "$HOME/projects"
}


case $OS in
  "Darwin"|"Linux")
    : # hokay
    ;;
  *)
    die "ERROR: unknown OS: \"${OS}\"; can't continue."
    ;;
esac

main "$@"
