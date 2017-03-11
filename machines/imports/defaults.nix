{ config, pkgs, ... }:

let
  dotfiles = import ../.. {};
  readFile = builtins.readFile;
in

# Stuff I want to config/install on every machine, regardless of type.
{

  imports = [
    ./nix.nix
    ./package-overrides.nix
    ./users.nix
    ./yubikey.nix
  ];

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

  services.xserver.enableCtrlAltBackspace = false;

  programs.bash.enableCompletion = true;

  programs.tmux = {
    enable = true;
    terminal = "screen-256color";
    keyMode = "vi";
    # this can go in my homedir
    #extraTmuxConf = builtins.readFile ../../cfg/.tmux.conf;
  };

  # this can go in my homedir
  #programs.ssh.extraConfig = readFile ../../cfg/.ssh/config;

  environment.variables = {
    EDITOR = "vim";
  };

  environment.systemPackages = with pkgs; [
    dotfiles.nix-home
    dotfiles.myVim
    acpi
    bc
    cabal-install
    ctags
    dnsutils
    dstat
    exercism
    file
    git
    haskellPackages.ghc
    haskellPackages.ghc-mod
    haskellPackages.hdevtools
    haskellPackages.hindent
    haskellPackages.hlint
    haskellPackages.ShellCheck
    html-tidy
    htop
    httpie
    iftop
    iotop
    jq
    jsonnet
    lsof
    mosh
    mtr
    nix-prefetch-scripts
    nix-repl
    nmap
    nodePackages.js-yaml
    nodePackages.jshint
    pciutils
    pv
    pwgen
    python27Full
    pythonPackages.autopep8
    pythonPackages.flake8
    pythonPackages.pep8
    pythonPackages.pylint
    pythonPackages.virtualenv
    pythonPackages.virtualenvwrapper
    socat
    stack
    sysstat
    tcpdump
    telnet
    tig
    tree
    unzip
    usbutils
    wakelan
    wget
  ];

  services.udev.packages = [
    dotfiles.steamcontroller-udev-rules
  ];
}
