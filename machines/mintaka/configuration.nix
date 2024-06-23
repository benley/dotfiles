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

  boot.extraModprobeConfig = ''
    options snd-hda-intel model=nofixup
    options v4l2loopback devices=1
  '';

  boot.extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback ];
  boot.kernelModules = [ "v4l2loopback" ];

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

  services.zfs.autoSnapshot.enable = false;

  # Make the cryptsetup password prompt readable
  console.earlySetup = true;

  networking.hostName = "mintaka";
  networking.hostId = "8425e349";

  networking.firewall.allowedUDPPorts = [
    7236   # miracast
  ];
  networking.firewall.allowedTCPPorts = [
    51413  # transmission
    7236   # miracast
    7250   # miracast
  ];

  networking.networkmanager.enable = true;

  console.font = "ter-132b";
  console.packages = [ pkgs.terminus_font ];

  # i18n.inputMethod.enabled = "ibus";
  # i18n.inputMethod.ibus.engines = with pkgs.ibus-engines; [ uniemoji ];

  # Accommodate Windows nonsesnse
  time.hardwareClockInLocalTime = true;

  # Allow changing timezone via dbus, so GeoIP location detection can work
  time.timeZone = null;

  system.stateVersion = "23.11";

  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.enableSSHSupport = true;

  programs.wireshark.enable = true;
  programs.wireshark.package = pkgs.wireshark;

  services.resolved.enable = true;
  services.resolved.extraConfig = ''
    MulticastDNS=true
  '';

  services.avahi.nssmdns4 = false;
  services.avahi.publish.addresses = false;
  services.avahi.publish.enable = false;

  services.openssh.enable = true;

  services.libinput.enable = true;
  services.libinput.touchpad.naturalScrolling = true;

  services.xserver = {
    dpi = 144;

    xkb.options = lib.concatStringsSep "," [
      "ctrl:nocaps"
      #"altwin:swap_alt_win"
    ];
 };

  hardware.trackpoint.enable = true;
  hardware.trackpoint.device = "TPPS/2 Elan TrackPoint";
  hardware.trackpoint.emulateWheel = true;

  # services.throttled.enable = true;

  hardware.graphics = {
    extraPackages = with pkgs;
        [ vaapiIntel libvdpau libvdpau-va-gl vaapiVdpau intel-media-driver ];
    extraPackages32 = with pkgs.pkgsi686Linux;
        [ vaapiIntel libvdpau libvdpau-va-gl vaapiVdpau ];
  };

  environment.variables = {
    # There is no dedicated i965 vpdau driver, so this tells libvpdau to use
    # libva as its backend.
    VDPAU_DRIVER = "va_gl";
  };

  environment.sessionVariables = {
    NIXOS_OZONE_WL = "1";
  };

  hardware.cpu.intel.updateMicrocode = true;

  # virtualisation.docker.enable = true;
  # virtualisation.docker.storageDriver = "zfs";
  virtualisation.podman.enable = true;

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

  services.powermate.enable = false;

  environment.systemPackages = [
    pkgs.moonlight-qt
    # pkgs.lutris
    # pkgs.zoom-us
    # pkgs.virtscreen
    pkgs.virt-manager
    pkgs.bespokesynth
    pkgs.blender
  ];

  # services.nfs.server = {
  #   enable = true;
  #   # hostName = "172.17.0.1";
  #   exports = ''
  #     /export/next *(rw,no_root_squash,insecure)
  #   '';
  #   extraNfsdConfig = ''
  #     vers2=on
  #   '';
  # };

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
          host = "benley@nyanbox";
          dataset = "nyanbox/backup/mintaka/home";
          plan = "1d=>1h,1w=>1d,1m=>1w,1y=>1m";
        };
        mbuffer.enable = true;
      };
    };
  };

  # security.pam.u2f = {
  #   enable = true;
  #   cue = true;
  #   authFile = ./u2f_keys;
  #   control = "requisite";
  #   appId = "pam://zoiks.net";
  # };

  services.tailscale.enable = true;

  services.printing.enable = true;

  virtualisation.libvirtd.enable = true;

  zramSwap.enable = true;
}
