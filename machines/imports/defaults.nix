{ config, pkgs, ... }:

{
  programs.bash.enableCompletion = true;
  programs.tmux.keyMode = "vi";

  environment.variables = {
    EDITOR = "vim";
  };

  environment.systemPackages = with pkgs; [
    bc
    cabal-install
    dstat
    file
    git
    htop
    httpie
    nix-prefetch-scripts
    nix-repl
    iftop
    iotop
    python27Full
    stack
    sysstat
    tig
    tmux
    vimHugeX
  ];
}
