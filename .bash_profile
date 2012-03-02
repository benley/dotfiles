os=$(uname)

case "$OS" in
  "Linux")
  ;;
  "Darwin")
    export PATH=/$HOME/bin:$HOME/Dropbox/macbin:/opt/local/bin:/opt/local/sbin:$PATH
    alias c='ssh catbus'
    alias irc='ssh catbus -t screen -rdU irc'
    export CLICOLOR=1
    # solarized-ish colors
    export LSCOLORS=gxfxbEaEBxxEhEhBaDaCaD
  ;;
esac
