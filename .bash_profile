if [[ $- =~ i && -x $(which keychain) ]]; then
  eval $(keychain --agents gpg --quick --eval --nogui)
fi

source "$HOME"/.bashrc
