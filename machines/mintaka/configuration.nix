# Mintaka: Lenovo Thinkpad X1 carbon, 6th gen (2018)

{ config, pkgs, lib, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../imports/defaults.nix
    ../imports/graphical.nix
    ../imports/redshift.nix
    ../imports/wacom.nix
  ];

  boot.kernelPackages = pkgs.linuxPackages_latest;

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

  boot.initrd.availableKernelModules = [
    "lz4" "lz4_compress" "z3fold"
  ];

  boot.supportedFilesystems = ["zfs"];
  boot.zfs.enableUnstable = false;  # zfs 0.8.1 is stable now, yay!

  services.zfs.autoSnapshot.enable = true;

  # Make the cryptsetup password prompt readable
  console.earlySetup = true;

  networking.hostName = "mintaka";
  networking.hostId = "8425e349";

  networking.firewall.allowedUDPPorts = [
    27031 27036  # Steam in-home streaming
  ];
  networking.firewall.allowedTCPPorts = [
    27036 27037  # Steam in-home streaming
    51413  # transmission
  ];

  networking.networkmanager.enable = true;

  console.font = "ter-132b";
  console.packages = [ pkgs.terminus_font ];
  i18n.inputMethod.enabled = "ibus";
  i18n.inputMethod.ibus.engines = with pkgs.ibus-engines; [ uniemoji ];

  # Accommodate Windows nonsesnse
  time.hardwareClockInLocalTime = true;

  # Allow changing timezone via dbus, so GeoIP location detection can work
  time.timeZone = null;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?

  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.enableSSHSupport = true;

  services.avahi.publish.enable = false;

  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;

  services.xserver = {
    libinput.enable = true;
    libinput.touchpad.naturalScrolling = true;

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

  hardware.cpu.intel.updateMicrocode = true;

  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";

  # evdev:atkbd:... modalias string comes from `evemu-describe /dev/input/event0`
  # (it comes from DMI data, you can probably also find it with `cat /sys/class/dmi/id/modalias`)
  services.udev.extraHwdb = ''
    # Remap PrtSc (which is bizarrely between right-alt and right-ctl) to right-meta
    evdev:atkbd:dmi:bvn*:bvr*:bd*:svn*:pn*:pvrThinkPadX1C*
      KEYBOARD_KEY_b7=rightmeta

    # Remap fn-f11 and fn-12 to something usable (XF86Launch1, XF86Launch2)
    evdev:name:ThinkPad Extra Buttons:dmi:bvn*:bvr*:bd*:svnLENOVO*:pn*
      KEYBOARD_KEY_49=prog1
      KEYBOARD_KEY_45=prog2
  '';

  services.fwupd.enable = true;

  services.logind.extraConfig = ''
    KillUserProcesses=yes
  '';

  services.flatpak.enable = true;

  services.znapzend = {
    enable = true;
    autoCreation = true;
    features.recvu = true;
    features.compressed = true;
    zetup = {
      "rpool/home" = {
        plan = "1h=>15min,1d=>1h,1w=>1d,1m=>1w,6m=>1m";
        destinations.nyanbox = {
          host = "benley@nyanbox.zoiks.net";
          dataset = "nyanbox/backup/mintaka/home";
          plan = "1d=>1h,1w=>1d,1m=>1w,1y=>1m";
        };
        mbuffer.enable = true;
      };
    };
  };

  services.tailscale.enable = true;

  services.printing.enable = true;
  services.printing.drivers = with pkgs; [ hplipWithPlugin ];

  virtualisation.libvirtd.enable = true;
}
