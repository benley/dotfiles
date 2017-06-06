{ config, pkgs, lib, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../imports/defaults.nix
    ../imports/graphical.nix
    ../imports/kde.nix
    ../imports/virtualbox.nix
    ../imports/wacom.nix
  ];

  #boot.kernelPackages = pkgs.linuxPackages_4_9;

  boot.loader.grub = {
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

  services.xserver.videoDrivers = [ "nvidia" ];

  # What does this actually do?
  powerManagement.enable = true;

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
}
