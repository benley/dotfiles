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

  hardware.nvidia.open = true;
  hardware.nvidia.modesetting.enable = true;
  hardware.nvidia.powerManagement.enable = true;

  # Workaround stupid nvidia thing where the system goes back to sleep
  # immediately after waking up
  # https://www.reddit.com/r/archlinux/comments/1oj3tlp/fix_nvidia_sleep_race_immediate_sleep_after_wake/
  systemd.services.systemd-suspend.environment.SYSTEMD_SLEEP_FREEZE_USER_SESSIONS = "false";
  systemd.services.systemd-hibernate.environment.SYSTEMD_SLEEP_FREEZE_USER_SESSIONS = "false";
  systemd.services.systemd-hybrid-sleep.environment.SYSTEMD_SLEEP_FREEZE_USER_SESSIONS = "false";
  systemd.services.systemd-suspend-then-hibernate.environment.SYSTEMD_SLEEP_FREEZE_USER_SESSIONS = "false";
  # if systemd-homed ever shows up, add SYSTEMD_HOME_LOCK_FREEZE_SESSION=false to it too

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
  # you can only reach this inside my network anyway
  services.openssh.settings.PasswordAuthentication = true;

  services.printing = {
    enable = true;
    drivers = [ pkgs.hplip ];
  };

  services.xserver = {
    dpi = 137;
    videoDrivers = [ "nvidia" ];
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

  services.fwupd.enable = true;

  services.tailscale.enable = true;

  time.hardwareClockInLocalTime = true;  # accommodate windows bullshit

  environment.systemPackages = with pkgs; [
    # dolphinEmuMaster
    mixxx
    qpwgraph
    r2modman
    # sidequest
    # satisfactorymodmanager # build broken 2025-09-07, should be fixed soon
    blender
    unityhub
    bottles
  ];

  services.prometheus.exporters.node = {
    enable = true;
    openFirewall = true;

    disabledCollectors = ["arp"];
  };

  hardware.spacenavd.enable = true;

  programs.gamescope.enable = true;
}
