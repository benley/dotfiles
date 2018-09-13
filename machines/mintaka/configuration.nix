# Mintaka: Lenovo Thinkpad X1 carbon, 6th gen (2018)

{ config, pkgs, lib, ... }:

with {
  # x1c_patched_dsdt = pkgs.makeInitrd {
  #   compressor = "cat";
  #   contents = [
  #     { object = ./dsdt.aml;
  #       symlink = "kernel/firmware/acpi/dsdt.aml";
  #     }
  #   ];
  # };
  x1c_patched_dsdt = pkgs.stdenv.mkDerivation {
    name = "x1c-patched-dsdt";
    src = ./.;
    buildInputs = [ pkgs.libarchive ];
    installPhase = ''
      mkdir -p kernel/firmware/acpi
      cp ${./dsdt.aml} kernel/firmware/acpi/dsdt.aml
      mkdir -p $out
      echo kernel/firmware/acpi/dsdt.aml | bsdcpio -v -o -H newc -R 0:0 > $out/x1c-dsdt.img
    '';
  };
};

{
  imports = [
    ./hardware-configuration.nix
    ../imports/defaults.nix
    ../imports/graphical.nix
    ../imports/redshift.nix
    ../imports/wacom.nix
    ../imports/workstuff.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelParams = [
    # Enable S0i3 sleep support, according to the arch wiki
    # "acpi.ec_no_wakeup=1"

    # Or, with the patched DSDT:
    "mem_sleep_default=deep"

    "zswap.enabled=1"
    "zswap.max_pool_percent=25"

    # These next two have to happen in postBootCommands because the
    # modules aren't compiled into the kernel
    # "zswap.zpool=z3fold"
    # "zswap.compressor=lz4"
  ];

  boot.postBootCommands = ''
    echo z3fold > /sys/module/zswap/parameters/zpool
    echo lz4 > /sys/module/zswap/parameters/compressor
  '';

  boot.initrd.prepend = ["${x1c_patched_dsdt}/x1c-dsdt.img"];

  boot.initrd.availableKernelModules = [
    "lz4" "lz4_compress" "z3fold"
  ];

  boot.supportedFilesystems = ["zfs"];
  boot.zfs.enableUnstable = true;

  # Make the cryptsetup password prompt readable
  boot.earlyVconsoleSetup = true;

  networking.hostName = "mintaka";
  networking.hostId = "8425e349";

  # ports for steam in-home streaming
  networking.firewall.allowedUDPPorts = [ 27031 27036 ];
  networking.firewall.allowedTCPPorts = [ 27036 27037 ];

  networking.networkmanager.enable = true;

  i18n.consoleFont = "ter-132b";
  i18n.consolePackages = [ pkgs.terminus_font ];

  # time.timeZone = "America/Los_Angeles";


  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?

  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.enableSSHSupport = true;

  services.avahi.publish.enable = true;
  services.avahi.publish.workstation = true;
  services.avahi.publish.userServices = true;

  # services.crashplan.enable = true;
  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;

  services.xserver = {
    libinput.enable = true;
    libinput.naturalScrolling = true;

    videoDrivers = [ "intel" ];

    dpi = 144;

    xkbOptions = lib.concatStringsSep "," [
      "ctrl:nocaps"
      #"altwin:swap_alt_win"
    ];
  };

  hardware.opengl = {
    extraPackages = with pkgs;
        [ vaapiIntel libvdpau libvdpau-va-gl vaapiVdpau ];
    extraPackages32 = with pkgs.pkgsi686Linux;
        [ vaapiIntel libvdpau libvdpau-va-gl vaapiVdpau ];

    s3tcSupport = true;
  };

  environment.variables = {
    # There is no dedicated i965 vpdau driver, so this tells libvpdau to use
    # libva as its backend.
    VDPAU_DRIVER = "va_gl";
  };

  hardware.bluetooth.enable = true;
  hardware.cpu.intel.updateMicrocode = true;

  # services.redshift = {
  #   latitude = "37.7749";
  #   longitude = "-122.4194";
  # };

  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";

  # evdev:atkbd:... modalias string comes from `evemu-describe /dev/input/event0` (it comes from DMI data, you can probably also find it with `cat /sys/class/dmi/id/modalias`)
  # Remap PrintScreen (which is bizarrely located between right-alt
  # and right-ctrl on this laptop) to right-meta
  services.udev.extraHwdb = ''
    evdev:atkbd:dmi:bvn*:bvr*:bd*:svn*:pn*:pvrThinkPadX1C*
      KEYBOARD_KEY_b7=rightmeta
  '';

  services.powermate.enable = true;

  services.udev.packages = [
    pkgs.thunderbolt
  ];
  environment.systemPackages = [
    pkgs.thunderbolt
  ];

  # Temporary fix for cpu throttling issues visible in the kernel log
  # (journalctl -k) by setting the same temperature limits used by
  # Windows
  # See https://wiki.archlinux.org/index.php/Lenovo_ThinkPad_X1_Carbon_(Gen_6)#Power_management.2FThrottling_issues
  systemd.services.cpu-throttling = {
    enable = true;
    description = "Set temp offset to 3°C, so the new trip point is 97°C";
    documentation = [
      "https://wiki.archlinux.org/index.php/Lenovo_ThinkPad_X1_Carbon_(Gen_6)#Power_management.2FThrottling_issues"
    ];
    path = [ pkgs.msr-tools ];
    script = "wrmsr -a 0x1a2 0x3000000";
    serviceConfig = {
      Type = "oneshot";
    };
    wantedBy = [ "timers.target" ];
  };

  systemd.timers.cpu-throttling = {
    enable = true;
    description = "Set cpu throttling threshold to 97°C";
    documentation = [
      "https://wiki.archlinux.org/index.php/Lenovo_ThinkPad_X1_Carbon_(Gen_6)#Power_management.2FThrottling_issues"
    ];
    timerConfig = {
      OnActiveSec = 60;
      OnUnitActiveSec = 60;
      Unit = "cpu-throttling.service";
    };
    wantedBy = [ "timers.target" ];
  };

}
