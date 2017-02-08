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
    # ../imports/i3.nix
    # ../imports/gnome.nix
    ../imports/kde.nix
    ../imports/package-overrides.nix
    #../imports/redshift.nix  # using the kde applet
    ../imports/virtualbox.nix
    #../imports/gpu-passthrough.nix
  ];

  boot.loader = {
    systemd-boot.enable = false;

    timeout = 60;

    grub = {
      device = "/dev/sda";
      enable = true;
      efiSupport = true;
      zfsSupport = true;
      splashImage = null;
      extraEntries = ''
        menuentry "Windows" {
          search --fs-uuid --set=root --hint-bios=hd1,gpt2 --hint-efi=hd1,gpt2 --hint-baremetal=ahci1,gpt2 3285-F79B
          chainloader /EFI/Microsoft/Boot/bootmgfw.efi
        }
      '';
    };

    efi = {
      efiSysMountPoint = "/boot/efi";
      canTouchEfiVariables = true;
    };
  };

  boot.supportedFilesystems = [ "zfs" ];

  networking.hostName = "ein";
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
    bc
    ctags
    dpkg
    dropbox
    dstat
    file
    firefox
    git
    glxinfo
    google-chrome
    htop
    iftop
    insync
    iotop
    lsof
    nix-repl
    pciutils
    slack
    steam
    sysstat
    tig
    tmux
    unzip
    usbutils
    vlc
    vimHugeX
    wget
    xorg.xdpyinfo
    xorg.xev
    xlsfonts
    xsettingsd  # So I can use dump_xsettings
  ];

  services.openssh.enable = true;

  services.printing.enable = true;

  services.xserver = {
    enable = true;
    videoDrivers = [ "nvidia" ];

    #displayManager.slim.enable = true;
    #displayManager.slim.theme = slimThemeVuizvui;

    # layout = "us";
    # xkbOptions = "eurosign:e";

    wacom.enable = true;
    inputClassSections = [
      # http://linuxwacom.sourceforge.net/wiki/index.php/Consumer_Tablet_ExpressKey_Mapping_Issue
      ''
        Identifier "Wacom Bamboo 16FG 4x5 Pad pad GNOME compatibility"
        MatchDriver "wacom"
        MatchProduct "Wacom Bamboo 16FG 4x5 Pad pad"

        Option "Button1" "1"
        Option "Button5" "2"
        Option "Button4" "3"
        Option "Button3" "4"
      ''
    ];
  };

  hardware.opengl.driSupport32Bit = true;

  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;

  # What does this actually do?
  powerManagement.enable = true;

  programs.bash.enableCompletion = true;

  nix = {
    nixPath = [
      "/nix/var/nix/profiles/per-user/root/channels/nixos"
      #"nixos-config=/etc/nixos/configuration.nix"
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
    publish.enable = true;
    publish.workstation = true;
    publish.userServices = true;
  };

  users.extraUsers.benley = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "vboxusers" ];
  };

  systemd.mounts = [
    { where = "/var/lib/docker";
      what = "pool0/docker";
      type = "zfs";
      requiredBy = [ "docker.service" ];
      before = ["docker.service"];
      after = ["zfs-import-pool0.service"];
    }
  ];

  virtualisation.docker = {
    enable = true;
    storageDriver = "zfs";
    socketActivation = false;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";

}
