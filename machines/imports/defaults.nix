{ config, pkgs, ... }:

let
  dotfiles = import ../.. {};
  readFile = builtins.readFile;
in

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

  programs.tmux = {
    enable = true;
    terminal = "screen-256color";
    keyMode = "vi";
    extraTmuxConf = builtins.readFile ../../.tmux.conf;
  };

  programs.ssh.extraConfig = readFile ../../ssh/config;

  environment.variables = {
    EDITOR = "vim";
  };

  environment.systemPackages = with pkgs; [
    dotfiles.nix-home
    acpi
    bc
    cabal-install
    ctags
    dstat
    file
    git
    htop
    httpie
    iftop
    iotop
    lsof
    nix-prefetch-scripts
    nix-repl
    pciutils
    python27Full
    stack
    sysstat
    tig
    unzip
    usbutils
    vimHugeX
    wget
  ];
}
