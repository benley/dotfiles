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

  steamcontroller-udev-rules = pkgs.writeTextFile {
    name = "steamcontroller-udev-rules";
    text = ''
      # This rule is needed for basic functionality of the controller in
      # Steam and keyboard/mouse emulation
      SUBSYSTEM=="usb", ATTRS{idVendor}=="28de", MODE="0666"

      # This rule is necessary for gamepad emulation
      KERNEL=="uinput", MODE="0660", GROUP="wheel" OPTIONS+="static_node=uinput"
      # systemd option not yet tested
      #KERNEL=="uinput", SUBSYSTEM="misc", TAG+="uaccess", TAG+="udev-acl"

      # Valve HID devices over USB hidraw
      KERNEL=="hidraw*", ATTRS{idVendor}=="28de", MODE="0666"

      # Valve HID devices over bluetooth hidraw
      KERNEL=="hidraw*", ATTRS{idVendor}=="*28DE:*", MODE="0666"

      # DualShock 4 over USB hidraw
      KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="05c4", MODE="0666"

      # DualShock 4 wireless adapter over USB hidraw
      KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="0ba0", MODE="0666"

      # DualShock 4 Slim over USB hidraw
      KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="09cc", MODE="0666"

      # DualShock 4 over bluetooth hidraw
      KERNEL=="hidraw*", KERNELS=="*054C:05C4*", MODE="0666"

      # DualShock 4 Slim over bluetooth hidraw
      KERNEL=="hidraw*", KERNELS=="*054C:09CC*", MODE="0666"
    '';
    destination = "/etc/udev/rules.d/99-steamcontroller.rules";
  };
in

{
  imports = [
    ./hardware-configuration.nix
    ../imports/defaults.nix
    ../imports/fonts.nix
    ../imports/kde.nix
    ../imports/nix.nix
    ../imports/package-overrides.nix
    ../imports/users.nix
    ../imports/virtualbox.nix
    ../imports/wacom.nix
  ];

  boot.kernelPackages = pkgs.linuxPackages_4_8;

  boot.kernelParams = [
    "nvidia-drm.modeset=1"
    "noveau.modeset=0"
    "rd.driver.blacklist=noveau"
  ];

  boot.loader = {
    systemd-boot.enable = false;

    timeout = 60;

    grub = {
      device = "/dev/sda";
      enable = true;
      efiSupport = true;
      gfxmodeEfi = "1024x768x32";
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
    dpkg
    dropbox
    firefox
    glxinfo
    google-chrome
    insync
    slack
    steam
    vlc
    xorg.xdpyinfo
    xorg.xev
    xlsfonts
    xsettingsd  # So I can use dump_xsettings
  ];

  services.crashplan.enable = true;

  services.nix-serve = {
    enable = true;
    port = 5150;
    secretKeyFile = "/etc/nix/signing-key.sec";
  };

  services.openssh.enable = true;

  services.printing.enable = true;

  services.udev.packages = [ steamcontroller-udev-rules ];

  services.xserver = {
    enable = true;
    videoDrivers = [ "nvidia" ];

    #displayManager.slim.enable = true;
    #displayManager.slim.theme = slimThemeVuizvui;

    # layout = "us";
    # xkbOptions = "eurosign:e";

  };

  hardware.opengl.driSupport32Bit = true;

  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;

  # What does this actually do?
  powerManagement.enable = true;

  services.avahi = {
    enable = true;
    ipv4 = true;
    ipv6 = true;
    nssmdns = true;
    publish.enable = true;
    publish.workstation = true;
    publish.userServices = true;
  };

  # services.kmscon.enable = true;

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
