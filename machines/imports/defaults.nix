{ config, pkgs, lib, ... }:

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

  boot.zfs.forceImportAll = false;
  boot.zfs.forceImportRoot = false;
  services.zfs.autoSnapshot = {
    enable = true;
    flags = "-k -p --utc";
    frequent = 4;  # 15-minutely snapshots
    daily = 7;
    hourly = 24;
    weekly = 4;
    monthly = 12;
  };

  nixpkgs.config.allowUnfree = true;

  # These have no effect on google-chrome (I think), just chromium
  nixpkgs.config.chromium = {
    gnomeSupport = true;
    enablePepperFlash = true;
    enablePepperPDF = true;
    enableWideVine = true;
  };

  # http://nicknovitski.com/vim-nix-syntax wtf
  nixpkgs.config.vim.ftNix = false;

  nixpkgs.config.permittedInsecurePackages = [
    "linux-4.13.16"
  ];

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

  services.openssh.forwardX11 = true;

  services.emacs.defaultEditor = true;
  services.emacs.install = true;
  services.emacs.package = (import ../../emacs.nix) {inherit pkgs;};

  environment.variables.PAGER = "eless";

  environment.systemPackages = with pkgs; [
    dotfiles.eless
    dotfiles.nix-home
    acpi
    awscli
    dotfiles.awsudo
    bazel
    binutils # strings, strip, ar, as, ...
    bc
    cabal-install
    ctags
    dnsutils
    dropbox-cli
    dstat
    ethtool
    exercism
    file
    gdb
    git
    gnupg
    go
    google-cloud-sdk
    gotags
    haskellPackages.ghc
    # haskellPackages.ghc-mod
    haskellPackages.hdevtools
    haskellPackages.hindent
    haskellPackages.hlint
    haskellPackages.ShellCheck
    html-tidy
    htop
    httpie
    iftop
    iotop
    iw
    jq
    jsonnet
    dotfiles.kubernetes-client
    keybase
    keybase-gui
    kops
    kubernetes-helm
    lastpass-cli
    lsof
    gnumake
    mosh
    mtr
    dotfiles.neovim
    nethogs
    nix-prefetch-scripts
    nix-repl
    nmap
    nodePackages.js-yaml
    nodePackages.jshint
    openssl
    #oraclejdk
    pandoc
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
    whois
    zip
  ];

  services.udev.packages = [
    dotfiles.steamcontroller-udev-rules
  ];

  i18n.defaultLocale = "en_US.UTF-8";
  i18n.consoleUseXkbConfig = true;

  # Populate /etc/X11 since you're going to look there anyway
  services.xserver.exportConfiguration = true;
  services.xserver.enableCtrlAltBackspace = false;
  services.xserver.layout = "us";

  services.avahi = {
    enable = true;
    ipv4 = true;
    ipv6 = true;
    nssmdns = true;
  };

  services.irqbalance.enable = true;

  time.timeZone = lib.mkDefault "America/New_York";

  services.keybase.enable = true;
  services.kbfs.enable = true;
}
