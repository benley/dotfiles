#!/bin/bash

#TODO(benley): --dryrun isn't actually implemented.

set -o errexit

OS=$(uname)
case $OS in
  "Darwin"|"Linux")
  : # Do nothing.
  ;;
  *)
  echo "ERROR: unknown OS: \"${OS}\"; can't continue." >> /dev/stderr
  exit 1
  ;;
esac

function LogError() {
  [[ $# -lt 1 ]] && LogError "LogError: no error text given"
  echo "ERROR: $(basename $0): $@" >> /dev/stderr
}

function PrintUsage() {
#FIXME(benley): implement this
:
}

function SameFile() {
  # Return 0 if $1 and $2 are hard or soft links to the same file.
  # (That is, if they point at the same inode, or are literally the same file.)
  case $OS in
    "Darwin")
      inode1=$(stat -L -f%i $1 2>/dev/null)
      inode2=$(stat -L -f%i $2 2>/dev/null)
      ;;
    "Linux")
      inode1=$(stat -L -c%i $1 2>/dev/null)
      inode2=$(stat -L -c%i $2 2>/dev/null)
      ;;
    *)
      echo "ERROR: unknown OS: \"$OS\"" >> /dev/stderr
  esac
  [[ $inode1 -eq $inode2 ]] && return 0 || return 1
}

function Symlink() {
  # Symlink $1 to $2.
  # If force=1, don't confirm before overwriting existing files.
  if ((FLAGS_force)); then
    ln -sfn "$1" "$2" || true
  else
    ln -sin "$1" "$2" || true
  fi
}

function SymlinkIfDiffer() {
  # If $1 and $2 are not the same file or symlinks to the same file, symlink $1
  # to $2.
  if ! SameFile "$1" "$2"; then
    Symlink "$1" "$2"
  fi
}

function DoCleanUps() {
  # Some cleanups that should only be needed once per machine.
  # This isn't quite right; readlink doesn't necessarily return a canonical
  # path, but $PWD/blabla is one.
  if [[ -L "$HOME/.pythonrc" ]] && \
     [[ $(readlink "$HOME/.pythonrc") == "$PWD/.pythonrc" ]]; then
    rm "$HOME/.pythonrc"
  fi
}

function main() {
  export FLAGS_force=0
  export FLAGS_dryrun=0
  #TODO(benley): is export necessary here?

  for arg in $@; do
    case $arg in
      "-f" | "--force")
        readonly FLAGS_force=1
      ;;
      "--dryrun")
        readonly FLAGS_dryrun=0
      ;;
      "-h"|"-?"|"--help")
        PrintUsage >> /dev/stdout
        exit 0
        ;;
      *)
        LogError "Unrecognized option: $arg"
        PrintUsage >> /dev/stderr
        exit 1
      ;;
    esac
  done
  
  set -o nounset
  mkdir -p "$HOME/bin"
  dotfiles="bin/* .inputrc .bash_logout .bash_profile .bashrc .dircolors .pythonrc.py
            .screenrc .tmux.conf .quiltrc-dpkg .Xresources"
  for file in $dotfiles; do
    src="$PWD/$file"
    dst="$HOME/$file"
    SymlinkIfDiffer "$PWD/$file" "$HOME/$file"
  done
  
  mkdir -p $HOME/.ssh
  SymlinkIfDiffer "$PWD/ssh/config" "$HOME/.ssh/config"
  SymlinkIfDiffer "$PWD/vim/vimrc" "$HOME/.vimrc"
  SymlinkIfDiffer "$PWD/vim/dotvim" "$HOME/.vim"
  
  DoCleanUps
}

main "$@"
