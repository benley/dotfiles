{ config, pkgs, lib, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../imports/defaults.nix
    ./modules/cadvisor.nix
    ./modules/factorio-server
    ./modules/grafana.nix
    ./modules/home-assistant.nix
    ./modules/keycloak.nix
    ./modules/metube.nix
    ./modules/minecraft-server
    ./modules/netbox.nix
    ./modules/nextcloud.nix
    ./modules/node-exporter.nix
    ./modules/nyanbox-backups.nix
    ./modules/oauth2-proxy.nix
    ./modules/paperless.nix
    ./modules/photoprism.nix
    ./modules/prometheus.nix
    ./modules/transmission.nix
    ./modules/vaultwarden.nix
  ];

  my.cadvisor.enable = true;
  my.factorio-server.enable = false;  # OFF
  my.grafana.enable = true;
  my.home-assistant.enable = true;
  my.keycloak.enable = true;
  my.metube.enable = true;
  my.minecraft-server.enable = false; # OFF
  my.netbox.enable = true;
  my.nextcloud.enable = false;        # OFF
  my.node-exporter.enable = true;
  my.nyanbox-backups.enable = false;  # OFF
  my.oauth2-proxy = {
    enable = true;
    enableRedis = true;
  };
  my.paperless.enable = true;
  my.photoprism.enable = false;       # OFF
  my.prometheus.enable = true;
  my.transmission.enable = true;
  my.vaultwarden.enable = true;

  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efis/nvme0n1p2";
  boot.loader.grub = {
    devices = [ "nodev" ];
    enable = true;
    zfsSupport = true;
    efiSupport = true;
  };

  boot.kernelModules = ["nct6775"]; # make /sys/class/hwmon/hwmon2 exist

  networking.hostName = "nyanbox";
  networking.hostId = "007f0101";

  # Use a consistent ipv6 source address.  This way nginx can use ipv6
  # when reverse proxying to home-assistant, which needs a
  # known source address to trust the X-Forwarded-For header.
  # also I need a static IP for DNS
  networking.interfaces.enp3s0f0.tempAddress = "disabled";

  services.znapzend = {
    enable = true;
    zetup = {
      "nyanbox/software" = {
        plan = "1w=>1d,1m=>1w,1y=>1m";
      };
      "nyanbox/media" = {
        plan = "1w=>1d,1m=>1w,1y=>1m";
      };
      "nyanbox/photos" = {
        plan = "1w=>1d,1m=>1w,1y=>1m";
      };
      "nyanbox/downloads" = {
        plan = "1w=>1d,1m=>1w,1y=>1m";
      };
      "nyanbox/books" = {
        plan = "1w=>1d,1m=>1w,1y=>1m";
      };
      "nyanbox/paperless" = {
        plan = "1w=>1d,1m=>1w,1y=>1m";
      };
      "nyanbox/home" = {
        plan = "1d=>1h,1w=>1d,1m=>1w,1y=>1m";
        recursive = true;
      };
      "rpool/nixos" = {
        plan = "1d=>1h,1w=>1d,1m=>1w,1y=>1m";
        recursive = true;
      };
    };
  };

  services.openssh.enable = true;

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      80 443
      139 445 # Samba
      8080 # shrug
    ];
    allowedUDPPorts = [
      67 68 # bootp
      137 138 # Samba
    ];
    allowedUDPPortRanges = [
      # Allow all the upnp nonsense that's impossible to handle correctly
      { from = 32767; to = 65535; }
    ];
    # Allow tftp from the edgerouter only
    extraCommands = ''
      iptables  -A nixos-fw -p udp -m udp --dport 69 -s 192.168.7.1/32 -j ACCEPT
      ip6tables -A nixos-fw -p udp -m udp --dport 69 -s fd00::/64 -j ACCEPT
    '';
  };

  programs.mosh.enable = true;  # this opens UDP 60000 to 61000 in the firewall
  programs.wireshark.enable = true;

  system.stateVersion = "18.03";

  virtualisation.docker.enable = true;

  # virtualisation.oci-containers.containers.hyperion = {
  #   image = "hyperion:2.0.12";
  #   volumes = ["hyperion-data:/var/lib/hyperion"];
  #   ports = ["8090:8090" "8092:8092" "19444:19444" "19400:19400" "19445:19445"];
  #   cmd = ["hyperiond" "-u" "/var/lib/hyperion" "-v"];
  # };

  services.prometheus.exporters.blackbox = {
    enable = true;
    configFile = ./blackbox-exporter.yml;
  };

  services.smartd = {
    enable = true;
    autodetect = false;
    # Don't check disks in sleep or standby mode, unless smartd has
    # already skipped the previous 25 checks for that reason.
    defaults.monitored = "-a -n standby,25";
    devices = [
      { device = "/dev/sda"; options = "-d sat"; }
      { device = "/dev/sdb"; options = "-d sat"; }
      { device = "/dev/sdc"; options = "-d sat"; }
      { device = "/dev/sdd"; options = "-d sat"; }
    ];
  };

  security.acme.defaults.email = "benley@zoiks.net";
  security.acme.acceptTerms = true;

  services.dnsmasq.enable = true;
  services.dnsmasq.settings.server = [
    "2001:4860:4860::8888"
    "2001:4860:4860::8844"
    "8.8.8.8"
    "8.8.4.4"
  ];

  services.nginx = {
    enable = true;
    resolver.addresses = [ "127.0.0.1" ];
    # proxyResolveWhileRunning = true;
    recommendedBrotliSettings = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    recommendedZstdSettings = true;

    virtualHosts = {
      "nyanbox.zoiks.net" = {
        default = true;
        enableACME = true;
        forceSSL = true;

        extraConfig = ''
          proxy_buffer_size 8k;
        '';

        locations."/" = {
          tryFiles = "$uri $uri/ =404";
        };
      };
    };
  };

  services.plex = {
    enable = true;
    openFirewall = true;
  };

  services.samba = {
    enable = true;
    # Broken?
    # syncPasswordsByPam = true;

    # reminder: Shares can be defined in other modules,
    #           like home-assistant.nix and paperless.nix

    shares.scratch = {
      path = "/zfs/nyanbox/scratch";
      "read only" = false;
    };

    shares.downloads = {
      path = "/zfs/nyanbox/downloads";
      "read only" = false;
    };

    shares.software = {
      path = "/zfs/nyanbox/software";
      "read only" = false;
    };

    shares.photos = {
      path = "/zfs/nyanbox/photos";
    };

    shares.media = {
      path = "/zfs/nyanbox/media";
      "read only" = false;
    };

    shares.archives = {
      path = "/zfs/nyanbox/archives";
    };

    shares.backup = {
      path = "/zfs/nyanbox/backup";
      "read only" = false;
    };

    extraConfig = ''
      [homes]
      read only = no
      guest ok = no
    '';
  };

  system.autoUpgrade = {
    enable = true;
    flake = "/home/benley/p/dotfiles/machines/nyanbox";
    flags = ["--update-input" "nixpkgs" "--commit-lock-file"];
  };

  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 14d";

  # Enable outbound NAT from nixos containers
  networking.nat = {
    enable = true;
    internalInterfaces = ["ve-+"];
    # Apparently this can be left blank for outbound NAT?
    # externalInterface = "enp3s0f0";
  };

  environment.systemPackages = with pkgs; [
    mbuffer  # for remote znapzend senders
  ];

  services.tailscale.enable = true;
  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = 1;
    "net.ipv6.conf.all.forwarding" = 1;
  };

  # tftpd for router config archive.
  # See also networking.firewall.extraCommands
  services.atftpd = {
    enable = true;
    root = "/srv/tftp";
  };

  services.cloudflare-dyndns = {
    enable = true;
    ipv4 = true;
    ipv6 = true;
    apiTokenFile = "/root/cfdyndns-token.txt";
    domains = ["nyanbox.zoiks.net"];
  };
}
