#!/bin/bash

set -o errexit

force=0
if [[ "$1" == "-f" ]] || [[ "$1" == "--force" ]]; then
  readonly force=1
fi

set -o nounset

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
  if ((force)); then
    ln -sfn "$1" "$2"
  else
    ln -sin "$1" "$2"
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
  if [[ -l "$HOME/.pythonrc" ]] && \
     [[ $(readlink "$HOME/.pythonrc") == "$PWD/.pythonrc" ]]; then
    rm "$HOME/.pythonrc"
  fi
}

mkdir -p "$HOME/bin"
for file in bin/* .inputrc .bash_logout .bash_profile .bashrc .dircolors .pythonrc.py .screenrc .tmuxrc; do
  src="$PWD/$file"
  dst="$HOME/$file"
  SymlinkIfDiffer "$PWD/$file" "$HOME/$file"
done

SymlinkIfDiffer "$PWD/vim/vimrc" "$HOME/.vimrc"
SymlinkIfDiffer "$PWD/vim/dotvim" "$HOME/.vim"

DoCleanUps
