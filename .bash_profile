OS=$(uname)

case "$OS" in
  "Linux")
  ;;
  "Darwin")
    export PATH=$HOME/bin:$HOME/Dropbox/macbin:/opt/local/bin:/opt/local/sbin:$PATH
    alias c='ssh catbus'
    export CLICOLOR=1
    # solarized-ish colors
    export LSCOLORS=gxfxbEaEBxxEhEhBaDaCaD
    [[ -f /opt/local/etc/bash_completion ]] && source /opt/local/etc/bash_completion
  ;;
esac
