OS=$(uname)

case "$OS" in
  "Linux")
  ;;
  "Darwin")
    export PATH=$HOME/bin:$HOME/Dropbox/macbin:/opt/local/bin:/opt/local/sbin:$PATH
    [[ -f /opt/local/etc/bash_completion ]] && source /opt/local/etc/bash_completion
  ;;
esac

source .bashrc
