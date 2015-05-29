# Work around gnome-terminal's bullshit
# https://bugs.launchpad.net/ubuntu/+source/gnome-terminal/+bug/1429584

if [[ -n $VTE_VERSION && $TERM = xterm ]]; then
  export TERM=xterm-256color
fi
