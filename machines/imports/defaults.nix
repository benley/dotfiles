{ config, pkgs, lib, ... }:

# Stuff I want to config/install on every machine, regardless of type.
{

  imports = [
    ./nix.nix
    ./users.nix

    # These have on/off toggles, so including them does not
    # automatically enable anything
    ../../modules/dunst.nix
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

  nixpkgs.overlays = import ../../nixpkgs-overlays.nix;
  nixpkgs.config = import ../../nixpkgs-config.nix;

  documentation.dev.enable = true;

  programs.bash.enableCompletion = true;
  programs.bash.vteIntegration = lib.mkForce false;  # I let home-manager handle this

  programs.iftop.enable = true;
  programs.mtr.enable = true;

  services.openssh.settings.X11Forwarding = true;
  services.openssh.settings.PasswordAuthentication = false;

  programs.nano.enable = false;

  environment.systemPackages = with pkgs; [
    acpi
    binutils # strings, strip, ar, as, ...
    bc
    dnsutils
    dstat
    ethtool
    file
    git
    gnupg
    htop
    iotop
    iw
    jq
    lsof
    mosh
    nethogs
    nixos-option
    nix-prefetch-scripts
    nixos-option
    nmap
    openssl
    pciutils
    pv
    pwgen
    socat
    sysstat
    tcpdump
    inetutils
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
  services.xserver.xkb.layout = "us";

  services.irqbalance.enable = true;

  time.timeZone = lib.mkDefault "America/New_York";

  services.logind.extraConfig = ''
    HandlePowerKey=suspend
  '';

  # https://yulistic.gitlab.io/2017/12/linux-keymapping-with-udev-hwdb/
  # https://unix.stackexchange.com/a/587975
  services.udev.extraHwdb = ''
    # Swap left/right buttons on some external mice (not all, since I have that left-handed one!)
    evdev:name:Logitech Wireless Mouse:*
      ID_INPUT_KEY=1
      KEYBOARD_KEY_90001=btn_right
      KEYBOARD_KEY_90002=btn_left

    evdev:name:Razer*Viper*
      KEYBOARD_KEY_90001=btn_right
      KEYBOARD_KEY_90002=btn_left
  '';

  networking.firewall.checkReversePath = lib.mkIf config.services.tailscale.enable "loose";
}
