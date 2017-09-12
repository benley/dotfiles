{ config, pkgs, lib, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../imports/defaults.nix
    ../imports/graphical.nix
    ../imports/kde.nix
    ../imports/redshift.nix
    ../imports/virtualbox.nix
    ../imports/wacom.nix
  ];

  boot.kernelPackages = pkgs.linuxPackages_4_12;
  boot.kernelParams = [
    "zswap.enabled=1"
    "zswap.compressor=lz4"
    "zswap.max_pool_percent=25"
  ];

  boot.loader.grub = {
    device = "/dev/sdb";
    enable = true;
    efiSupport = true;
    gfxmodeEfi = "1024x768x32";
    zfsSupport = true;
    splashImage = null;
    useOSProber = true;
  };

  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.loader.efi.canTouchEfiVariables = true;

  boot.supportedFilesystems = [ "zfs" ];

  networking.hostName = "ein";
  networking.hostId = "40ec1be4";

  networking.firewall.enable = false;

  services.crashplan.enable = true;

  services.nix-serve = {
    enable = true;
    port = 5150;
    secretKeyFile = "/etc/nix/signing-key.sec";
  };

  services.openssh.enable = true;

  services.printing = {
    enable = true;
    drivers = [ pkgs.hplipWithPlugin ];
    gutenprint = true;
  };

  services.xserver = {
    videoDrivers = [ "nvidia" ];
    screenSection = ''
      Option "metamodes" "nvidia-auto-select +0+0 {ForceCompositionPipeline=On}"
    '';
  };

  services.avahi.publish.enable = true;
  services.avahi.publish.workstation = true;
  services.avahi.publish.userServices = true;

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

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.03";

  services.synergy.server.enable = true;
  services.synergy.server.enableCrypto = true;
  services.synergy.server.configFile = ./synergy.conf;

  hardware.opengl = {
     extraPackages = with pkgs; [ vaapiVdpau ];
     extraPackages32 = with pkgs.pkgsi686Linux; [ vaapiVdpau ];
  };

  environment.variables = {
    VDPAU_DRIVER = "nvidia";
    LIBVA_DRIVER_NAME = "vdpau";
  };
}
