{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../imports/defaults.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  networking.hostId = "9dd7f426";
  networking.hostName = "stumpy";
  # networking.wireless.enable = true;

  networking.useDHCP = false;
  networking.interfaces.enp4s0.useDHCP = true;
  networking.interfaces.wlp2s0.useDHCP = true;

  networking.firewall = {
    # logRefusedConnections = true;
    # logRefusedPackets = true;
    # logRefusedUnicastsOnly = false;
    # logReversePathDrops = true;

    allowedTCPPorts = [
      8123 # home-assistant
      8989 # WeMo device callback
    ];
    allowedUDPPorts = [
      67 68 # bootp
      137 138 # Samba
    ];
    allowedUDPPortRanges = [
      # Allow all the upnp nonsense that's impossible to handle correctly
      { from = 32767; to = 65535; }
    ];
  };

  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [
    wget vim htop git
  ];

  programs.mtr.enable = true;

  services.openssh.enable = true;

  users.users.benley = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  system.stateVersion = "20.03";

  services.zfs.trim.enable = true;

  programs.tmux.enable = true;
  
  services.emacs.install = true;
  services.emacs.defaultEditor = true;

  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";
  virtualisation.docker.autoPrune.enable = true;

  system.autoUpgrade = {
    enable = true;
  };

  services.prometheus.exporters.node = {
    enable = true;
    extraFlags = [
      # "--collector.textfile.directory=/var/lib/node-exporter/textfile"
      "--collector.filesystem.ignored-fs-types=^(sysfs|procfs|autofs|cgroup|devpts|nsfs|aufs|tmpfs|overlay|fuse|fuse\.lxc|mqueue)$"
      "--collector.filesystem.ignored-mount-points=^(/rootfs|/host)?/(sys|proc|dev|host|etc)($|/)"
    ];
    disabledCollectors = [ "timex" ];
    openFirewall = true;  # allow port 9100
  };

  services.udev.packages = [
    # I think this could go in 99-local.rules but I'm not entirely sure
    (pkgs.writeTextFile {
      name = "hubZ-udev-rules";
      text = builtins.readFile ../nyanbox/zigbee-zwave-udev.rules;
      destination = "/etc/udev/rules.d/70-hubZ.rules";
    })
  ];

  docker-containers.home-assistant = {
    image = "homeassistant/home-assistant:0.111.4";
    volumes = ["/var/lib/hass:/config"];
    # ports = ["8123:8123"];
    extraDockerOptions = [
      "--network=host"
      "--init"
      "--device=/dev/zigbee"
      "--device=/dev/zwave"
    ];
  };

}
