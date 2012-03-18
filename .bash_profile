OS=$(uname)

case "$OS" in
  "Linux")
  ;;
  "Darwin")
    export PATH=$HOME/bin:$HOME/Dropbox/macbin:/opt/local/bin:/opt/local/sbin:$PATH
    [[ -f /opt/local/etc/bash_completion ]] && source /opt/local/etc/bash_completion
  ;;
esac

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

source .bashrc
