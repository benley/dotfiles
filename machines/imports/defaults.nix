{ config, pkgs, lib, ... }:

# Stuff I want to config/install on every machine, regardless of type.
{

  imports = [
    ./nix.nix
    ./package-overrides.nix
    ./users.nix
    ./yubikey.nix

    # These have on/off toggles, so including them does not
    # automatically enable anything
    ../../modules/dunst.nix
    ../../modules/fancontrol.nix
    ../../modules/hddfancontrol.nix
    ../../modules/powermate.nix
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

  nixpkgs.overlays = [
    (import ../../overlays/default.nix)
    (import ../../overlays/emacs.nix)
  ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.oraclejdk.accept_license = true;
  # These have no effect on google-chrome (I think), just chromium
  # nixpkgs.config.chromium = {
  #   gnomeSupport = true;
  #   enablePepperFlash = true;
  #   enablePepperPDF = true;
  #   enableWideVine = true;
  # };

  # http://nicknovitski.com/vim-nix-syntax wtf
  nixpkgs.config.vim.ftNix = false;

  documentation.dev.enable = true;

  programs.bash.enableCompletion = true;

  programs.iftop.enable = true;
  programs.mtr.enable = true;

  programs.tmux = {
    enable = true;
    terminal = "screen-256color";
    keyMode = "emacs";
    # this can go in my homedir
    #extraTmuxConf = builtins.readFile ../../cfg/.tmux.conf;
  };

  # this can go in my homedir
  #programs.ssh.extraConfig = readFile ../../cfg/.ssh/config;

  services.openssh.forwardX11 = true;
  services.openssh.passwordAuthentication = false;

  services.emacs.defaultEditor = true;
  services.emacs.install = true;
  # now handled via overlay
  # services.emacs.package = (import ../../emacs.nix) {inherit pkgs;};

  # environment.variables.PAGER = "eless";

  environment.systemPackages = with pkgs; [
    eless
    nix-home
    acpi
    awscli
    binutils # strings, strip, ar, as, ...
    bc
    cabal-install
    cabal2nix
    ctags
    dnsutils
    dropbox-cli
    dstat
    ethtool
    exercism
    file
    gdb
    gitFull
    gnupg
    go
    google-cloud-sdk
    gotags
    haskellPackages.ghc
    # haskellPackages.ghc-mod  # broken?
    # haskellPackages.hdevtools # broken?
    haskellPackages.hindent
    haskellPackages.hlint
    haskellPackages.ShellCheck
    html-tidy
    htop
    httpie
    # iftop  # see programs.iftop.enable
    imagemagick  # so emacs can resize images
    iotop
    iw
    jq
    jsonnet
    lastpass-cli
    lsof
    gnumake
    mosh
    # mtr   # see programs.mtr.enable
    nethogs
    nix-prefetch-scripts
    nmap
    nodePackages.js-yaml
    nodePackages.jshint
    openssl
    pandoc
    pciutils
    pv
    pwgen
    python27Full
    python3Full
    pythonPackages.autopep8
    pythonPackages.flake8
    pythonPackages.pep8
    python3Packages.pylint
    pythonPackages.virtualenv
    pythonPackages.virtualenvwrapper
    socat
    stack
    sysstat
    tcpdump
    telnet
    terminfo-extras  # my custom stuff
    tig
    tree
    unrar
    unzip
    usbutils
    wakelan
    wget
    whois
    zip
  ];

  services.udev.packages = [
    pkgs.adafruit-udev-rules
    pkgs.steamcontroller-udev-rules
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

  services.logind.extraConfig = ''
    HandlePowerKey=suspend
  '';
}
