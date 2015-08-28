#!bash
if [[ $- =~ i
   && $(type -t keychain) != ""
   && $(type -t gpg-agent) != ""
   ]];
then
  eval $(keychain --agents gpg --quick --eval --nogui)
fi

source "$HOME"/.bashrc
