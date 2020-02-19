{ config, pkgs, lib, ... }:

# Stuff I want to config/install on every machine, regardless of type.
{

  imports = [
    ./nix.nix
    ./package-overrides.nix
    ./users.nix

    # These have on/off toggles, so including them does not
    # automatically enable anything
    ../../modules/dunst.nix
    ../../modules/fancontrol.nix
    ../../modules/hddfancontrol.nix
    ../../modules/powermate.nix
    ../../modules/mosquitto-exporter.nix
  ];

  boot.zfs.forceImportAll = false;
  boot.zfs.forceImportRoot = false;
  services.zfs.autoSnapshot = {
    enable = lib.mkDefault false;
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
    extraConfig = builtins.readFile ../../cfg/.tmux.conf;
  };

  # this can go in my homedir
  #programs.ssh.extraConfig = readFile ../../cfg/.ssh/config;

  services.openssh.forwardX11 = true;
  services.openssh.passwordAuthentication = false;

  services.emacs.defaultEditor = true;
  services.emacs.install = true;
  services.emacs.package = lib.mkDefault pkgs.basicEmacs;

  # environment.variables.PAGER = "eless";

  environment.systemPackages = with pkgs; [
    eless
    nix-home
    acpi
    binutils # strings, strip, ar, as, ...
    bc
    dnsutils
    dstat
    ethtool
    file
    gnupg
    htop
    httpie
    iotop
    iw
    jq
    lsof
    mosh
    nethogs
    nix-prefetch-scripts
    nmap
    openssl
    pciutils
    pv
    pwgen
    socat
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
  console.useXkbConfig = true;

  # Populate /etc/X11 since you're going to look there anyway
  services.xserver.exportConfiguration = true;
  services.xserver.enableCtrlAltBackspace = false;
  services.xserver.layout = "us";

  services.avahi = {
    enable = true;
    ipv4 = true;
    ipv6 = true;
    nssmdns = true;
    publish.enable = true;
    publish.addresses = true;
  };

  services.irqbalance.enable = true;

  time.timeZone = lib.mkDefault "America/New_York";

  services.logind.extraConfig = ''
    HandlePowerKey=suspend
  '';
}
