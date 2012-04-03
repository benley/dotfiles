#!/bin/bash -x

set -o errexit

LN="ln -sih"
if [[ "$1" == "-f" ]] || [[ "$1" == "--force" ]]; then
  LN="ln -sfn"
fi

set -o nounset

for file in .bash_logout .bash_profile .bashrc .dircolors .pythonrc.py .screenrc .tmuxrc; do
  $LN "$PWD/$file" "$HOME/$file"
done

mkdir -p "$HOME/bin"
for file in bin/*; do
  $LN "$PWD/$file" "$HOME/$file"
done

$LN "$PWD/vim/vimrc" "$HOME/.vimrc"
$LN "$PWD/vim/dotvim" "$HOME/.vim"
