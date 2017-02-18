#!bash
if [[ $- =~ i
   && $(type -t keychain) != ""
   && $(type -t gpg-agent) != ""
   ]];
then
  eval $(keychain --agents gpg --quick --eval --nogui)
fi

source ~/.bashrc

((RANDOM % 100 < 10)) && [[ $(type -t doge != "") ]] && doge

: # always end with $?=0
