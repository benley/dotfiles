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

  boot.kernelParams = [
    "zswap.enabled=1"
    "zswap.compressor=lz4"
    "zswap.max_pool_percent=25"
  ];

  boot.loader.grub = {
    device = "nodev";
    enable = true;
    efiSupport = true;
    # This motherboard's UEFI behaves badly and ignores BootOrder if
    # /EFI/Boot/bootx64.efi exists.  If that path doesn't exist, Windows
    # updates will eventually put a copy of the Windows bootloader there, so to
    # prevent Windows from taking over the boot process we'll have to install
    # Grub at that location.
    efiInstallAsRemovable = true;
    gfxmodeEfi = "1024x768x32";
    zfsSupport = true;
    splashImage = null;
    useOSProber = true;
    default = 2;  # boot windows by default for now
  };

  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  boot.supportedFilesystems = [ "zfs" "ntfs-3g" ];

  networking.hostName = "ein";
  networking.hostId = "40ec1be4";

  networking.firewall.enable = false;

  services.zfs.autoSnapshot.enable = true;

  services.openssh.enable = true;

  services.printing = {
    enable = true;
    drivers = [ pkgs.hplipWithPlugin ];
  };

  services.xserver = {
    dpi = 137;

    videoDrivers = [ "nvidia" ];

    # nvidia driver doesn't support wayland.  sigh
    displayManager.gdm.wayland = false;

    # ForceCompositionPipeline supposedly reduces screen tearing
    screenSection = ''
      Option "metamodes" "nvidia-auto-select +0+0 {ForceCompositionPipeline=On}"
    '';
    # This is supposed to help with DDC but it doesn't seem to work:
    #deviceSection = ''
    #  Option "RegistryDwords" "RMUseSwI2c=0x01; RMI2cSpeed=100"
    #'';

    # xkbOptions = "altwin:swap_alt_win";
  };

  systemd.mounts = [{
    where = "/var/lib/docker";
    what = "pool0/docker";
    type = "zfs";
    requiredBy = [ "docker.service" ];
    before = ["docker.service"];
    after = ["zfs-import-pool0.service"];
  }];

  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";

  system.stateVersion = "17.03";

  hardware.opengl = {
     extraPackages = with pkgs; [ vaapiVdpau ];
     extraPackages32 = with pkgs.pkgsi686Linux; [ vaapiVdpau ];
  };

  hardware.pulseaudio.extraConfig = ''
    unload-module module-suspend-on-idle
  '';

  environment.variables = {
    VDPAU_DRIVER = "nvidia";
    LIBVA_DRIVER_NAME = "vdpau";
  };
}
