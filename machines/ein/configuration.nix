{ config, pkgs, lib, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../imports/defaults.nix
    ../imports/graphical.nix
    ../imports/redshift.nix
    ../imports/wacom.nix
  ];

  boot.kernelModules = [ "nct6775" "drivetemp" ];

  # boot.extraModprobeConfig = ''
  #   options nvidia_drm fbdev=1 modeset=1
  # '';

  boot.loader.grub = {
    device = "nodev";
    enable = true;
    efiSupport = true;
    zfsSupport = true;
    useOSProber = true;
    # default = 2;  # boot windows by default for now
    memtest86.enable = true;
  };

  boot.loader.systemd-boot.enable = false;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  boot.supportedFilesystems = [ "zfs" "ntfs-3g" ];

  networking.hostName = "ein";
  networking.hostId = "40ec1be4";

  networking.firewall.enable = false;

  services.zfs.autoSnapshot.enable = true;

  services.openssh.enable = true;

  services.printing = {
    enable = true;
    drivers = [ pkgs.hplip ];
  };

  services.xserver = {
    dpi = 137;

    videoDrivers = [ "nvidia" ];

    # nvidia driver doesn't support wayland.  sigh
    # displayManager.gdm.wayland = false;

    # ForceCompositionPipeline supposedly reduces screen tearing
    # screenSection = ''
    #   Option "metamodes" "nvidia-auto-select +0+0 {ForceCompositionPipeline=On}"
    # '';
    # This is supposed to help with DDC but it doesn't seem to work:
    #deviceSection = ''
    #  Option "RegistryDwords" "RMUseSwI2c=0x01; RMI2cSpeed=100"
    #'';

    # xkbOptions = "altwin:swap_alt_win";
  };

  systemd.mounts = [{
    where = "/var/lib/docker";
    what = "fastroot/docker";
    type = "zfs";
    requiredBy = [ "docker.service" ];
    before = ["docker.service"];
    after = ["zfs-import-pool0.service"];
  }];

  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";

  system.stateVersion = "17.03";

  hardware.pulseaudio.extraConfig = ''
    unload-module module-suspend-on-idle
  '';

  # environment.variables = {
  #   VDPAU_DRIVER = "nvidia";
  #   LIBVA_DRIVER_NAME = "vdpau";
  # };

  services.fwupd.enable = true;

  services.tailscale.enable = true;

  time.hardwareClockInLocalTime = true;  # accommodate windows bullshit

  programs.alvr.enable = true;
  programs.alvr.openFirewall = true;

  environment.systemPackages = with pkgs; [
    mixxx
    sidequest
    qpwgraph
    pulseaudio
  ];

  hardware.pulseaudio.enable = lib.mkForce false;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  services.prometheus.exporters.node = {
    enable = true;
    openFirewall = true;

    disabledCollectors = ["arp"];
  };

}
