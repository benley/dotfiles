{ config, pkgs, ... }:

# Stuff I want to config/install on every machine, regardless of type.
{
  nixpkgs.config = {
    allowUnfree = true;

    # These have no effect on google-chrome (I think), just chromium
    chromium = {
      gnomeSupport = true;
      enablePepperFlash = true;
      enablePepperPDF = true;
      enableWideVine = true;
    };

    # vim.gui = "gtk3";  # gtk3 gets really weird if you're not running gnome
  };


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
    wget
  ];
}
