{ config, pkgs, lib, ... }:

let
  randrHeads = config.services.xserver.xrandrHeads;

  # NixOS theme with a dark background and white text
  slimThemeVuizvui = pkgs.stdenv.mkDerivation {
    name = "nixos-theme-vuizvui";
    src = pkgs.slimThemes.nixosSlim;
    phases = [ "unpackPhase" "patchPhase" "installPhase" ];
    patchPhase = let
      headFactor = if randrHeads == [] then 1 else lib.length randrHeads;
      centerLeft = 100 / (headFactor * 2);
    in ''
      ${pkgs.imagemagick.out}/bin/mogrify \
        -fill '#080010' -draw 'color 0,0 reset' \
        share/slim/themes/nixos-slim-testing/background.png
      ${pkgs.imagemagick.out}/bin/mogrify \
        -negate -region 100x110+0+0 -negate -fill white -colorize 20% \
        share/slim/themes/nixos-slim-testing/panel.png
      sed -i \
        -e 's/^\([a-z_]\+_x[^0-9]*\)[0-9]\+%/\1${toString centerLeft}%/' \
        share/slim/themes/nixos-slim-testing/slim.theme
      cat >> share/slim/themes/nixos-slim-testing/slim.theme <<EOF
      session_x      ${toString centerLeft}%
      msg_color      #ffffff
      username_color #ffffff
      password_color #ffffff
      input_color    #ffffff
      EOF
    '';
    installPhase = ''
      cp -R share/slim/themes/nixos-slim-testing "$out"
    '';
  };

in

{
  imports = [
    ./hardware-configuration.nix
    ../imports/fonts.nix
    ../imports/i3.nix
    ../imports/package-overrides.nix
    ../imports/redshift.nix
    ../imports/virtualbox.nix
  ];

  boot.loader = {
    systemd-boot.enable = false;

    grub = {
      device = "/dev/sda";
      enable = true;
      efiSupport = true;
      zfsSupport = true;
    };

    efi = {
      efiSysMountPoint = "/boot/efi";
      canTouchEfiVariables = true;
    };
  };


  boot.supportedFilesystems = [ "zfs" ];

  networking.hostName = "ein"; # Define your hostname.
  networking.hostId = "40ec1be4";

  networking.firewall = {
    enable = false;
    allowPing = true;
    allowedTCPPorts = [ 22 ];
  };

  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [
    acpi
    dstat
    file
    glxinfo
    htop
    iotop
    lsof
    pciutils
    sysstat
    tmux
    usbutils
    vimHugeX
    xlsfonts
  ];

  services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  services.xserver = {
    enable = true;
    videoDrivers = [ "nvidia" ];

    displayManager.slim.enable = true;
    displayManager.slim.theme = slimThemeVuizvui;

    # layout = "us";
    # xkbOptions = "eurosign:e";
  };

  hardware.opengl.driSupport32Bit = true;

  hardware.pulseaudio.enable = true;

  # What does this actually do?
  powerManagement.enable = true;

  programs.bash.enableCompletion = true;

  nix = {
    nixPath = [
      "/nix/var/nix/profiles/per-user/root/channels/nixos"
    #  #"nixos-config=/etc/nixos/configuration.nix"
      "nixos-config=/home/benley/p/dotfiles/machines/ein/configuration.nix"
      "/nix/var/nix/profiles/per-user/root/channels"
    ];
    buildCores = 0;
    daemonIONiceLevel = 7;
    daemonNiceLevel = 10;
    useSandbox = true;
    extraOptions = ''
      auto-optimise-store = true
    '';
    trustedBinaryCaches = [
      https://cache.nixos.org
      https://hydra.nixos.org
    ];
    binaryCachePublicKeys = [
      "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
    ];
  };

  services.avahi = {
    enable = true;
    ipv4 = true;
    ipv6 = true;
    nssmdns = true;
  };

  users.extraUsers.benley = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "vboxusers" ];
  };

  virtualisation.docker = {
    enable = true;
    storageDriver = "zfs";
    socketActivation = false;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";

}
