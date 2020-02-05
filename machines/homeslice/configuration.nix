{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../imports/defaults.nix
  ];

  swapDevices = [ { device = "/var/cache/swapfile"; size = 1024; } ];

  # Use the extlinux boot loader. (NixOS wants to enable GRUB by default)
  boot.loader.grub.enable = false;
  # Enables the generation of /boot/extlinux/extlinux.conf
  boot.loader.generic-extlinux-compatible.enable = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = ["cma=32M"];

  networking.hostName = "homeslice";

  services.avahi.publish.enable = true;
  services.avahi.publish.addresses = true;

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

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.eth0.useDHCP = true;
  networking.interfaces.wlan0.useDHCP = true;

  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [
    git
    raspberrypi-tools
  ];

  services.emacs.defaultEditor = true;
  services.emacs.install = true;

  services.openssh.enable = true;

  system.stateVersion = "19.09";

  virtualisation.docker.enable = true;

  docker-containers.home-assistant = {
    image = "homeassistant/raspberrypi3-homeassistant:0.105.0";
    volumes = ["/var/lib/hass:/config"];
    # ports = ["8123:8123"];
    extraDockerOptions = [
      "--network=host"
      "--init"
      "--device=/dev/zigbee"
      "--device=/dev/zwave"
    ];
  };

  services.udev.packages = [

    # I think this could go in 99-local.rules but I'm not entirely sure
    (pkgs.writeTextFile {
      name = "hubZ-udev-rules";
      text = builtins.readFile ../nyanbox/zigbee-zwave-udev.rules;
      destination = "/etc/udev/rules.d/70-hubZ.rules";
    })

  ];
}
