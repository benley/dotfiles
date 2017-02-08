{ config, pkgs, ... }:

{
  programs.bash.enableCompletion = true;
  programs.tmux.keyMode = "vi";

  environment.variables = {
    EDITOR = "vim";
  };

  environment.systemPackages = with pkgs; [
    bc
    file
    git
    htop
    httpie
    tig
    tmux
    vimHugeX
  ];
}
