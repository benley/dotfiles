{ config, pkgs, lib, ... }:

with rec {
  node-exporter-textfile-collector-scripts = pkgs.fetchFromGitHub {
    owner = "prometheus-community";
    repo = "node-exporter-textfile-collector-scripts";
    rev = "34dd42ee2cf5bf1ffcfdea5a3599130f146b88fc";
    sha256 = "1vwhj6n4sh2ggigmhac8qzsai3mm020dpp5phwixvifi6jv57sid";
  };
};

{
  imports = [
    ./hardware-configuration.nix
    ../imports/defaults.nix
    ./modules/grafana.nix
    ./modules/home-assistant.nix
    ./modules/keycloak.nix
    ./modules/nyanbox-backups.nix
    ./modules/netbox.nix
    ./modules/oauth2_proxy.nix
    ./modules/paperless.nix
    ./modules/photoprism.nix
    ./modules/prometheus.nix
    ./modules/transmission.nix
    ./modules/vaultwarden.nix
  ];

  my.grafana.enable = true;
  my.home-assistant.enable = true;
  my.keycloak.enable = true;
  my.netbox.enable = true;
  my.oauth2_proxy.enable = true;
  my.paperless.enable = true;
  my.photoprism.enable = false;
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

  services.zfs.autoSnapshot.enable = false;
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
    };
  };

  services.openssh.enable = true;

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      80 443
      139 445 # Samba
      25565 # minecraft
      27015 # factorio
    ];
    allowedUDPPorts = [
      67 68 # bootp
      137 138 # Samba
      34197 # Factorio
    ];
    allowedUDPPortRanges = [
      # Allow all the upnp nonsense that's impossible to handle correctly
      { from = 32767; to = 65535; }
    ];
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

  # virtualisation.oci-containers.containers.factorio = {
  #   image = "factoriotools/factorio:1.1.76";
  #   volumes = ["/var/lib/factorio:/factorio"];
  #   ports = ["34197:34197/udp" "27015:27015/tcp"];
  #   environment = {
  #     SAVE_NAME = "space exploration 0.6";
  #     LOAD_LATEST_SAVE = "false";
  #     USERNAME = "benley";
  #     TOKEN = builtins.readFile ./factorio-token.txt;
  #     UPDATE_MODS_ON_START = "false";
  #   };
  # };

  services.prometheus.exporters.node = {
    enable = true;
    extraFlags = [
      "--collector.textfile.directory=/var/lib/node-exporter/textfile"
      "--collector.filesystem.ignored-fs-types=^(sysfs|procfs|autofs|cgroup|devpts|nsfs|aufs|tmpfs|overlay|fuse|fuse\.lxc|mqueue)$"
      "--collector.filesystem.ignored-mount-points=^(/rootfs|/host)?/(sys|proc|dev|host|etc)($|/)"

    ];
    disabledCollectors = [ "timex" ];
    openFirewall = true;  # allow port 9100
  };

  services.prometheus.exporters.blackbox = {
    enable = true;
    configFile = ./blackbox-exporter.yml;
  };

  services.cron.systemCronJobs = [
    ''
      * * * * * root mkdir -p /var/lib/node-exporter/textfile; cd /var/lib/node-exporter/textfile; PATH=${pkgs.smartmontools}/bin:$PATH ${./smartmon-textfile.sh} | ${pkgs.moreutils}/bin/sponge smartmon.prom
      * * * * * root mkdir -p /var/lib/node-exporter/textfile; ${pkgs.ipmitool}/bin/ipmitool sensor | ${pkgs.gawk}/bin/awk -f ${node-exporter-textfile-collector-scripts}/ipmitool | ${pkgs.moreutils}/bin/sponge /var/lib/node-exporter/textfile/ipmitool.prom
    ''
  ];

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
    upstreams = {
      nextcloud.servers = { "192.168.7.181:9001" = {}; };
    };
    recommendedProxySettings = true;

    # TODO: in theory the stuff that oauth2_proxy puts in the root extraConfig
    # could actually go in the outer nginx "server" section, and then I
    # wouldn't have to hack it into each subpath like I'm doing here.  I think
    # it would just require adding "auth_request off" to the the location block
    # for /.well-known/acme-challenge to keep ACME stuff working.
    virtualHosts = {

      "nextcloud.zoiks.net" = {
        enableACME = true;
        forceSSL = true;

        extraConfig = ''
          proxy_buffer_size 8k;
        '';

        locations."/" = {
          proxyPass = "http://nextcloud/";
          proxyWebsockets = true;
          extraConfig = ''
          '';
        };

        locations."/.well-known/carddav" = {
          return = "301 $scheme://$host/remote.php/dav";
        };

        locations."/.well-known/caldav" = {
          return = "301 $scheme://$host/remote.php/dav";
        };
      };

      "nyanbox.zoiks.net" = let rootExtraConfig = config.services.nginx.virtualHosts."nyanbox.zoiks.net".locations."/".extraConfig; in {
        default = true;  # this is the default vhost
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

    # reminder: shares can be defined in other modules, like home-assistant.nix

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

    shares.paperless_consume = {
      path = "/var/lib/paperless/consume";
      "read only" = false;
      "force create mode" = 660;
      "force directory mode" = 770;
    };

    extraConfig = ''
      [homes]
      read only = no
      guest ok = no
    '';
  };

  system.autoUpgrade.enable = true;

  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 14d";

  containers.minecraft = {
    config = import ./minecraft-container.nix;
    autoStart = false;
    bindMounts = {
      "/var/lib/minecraft" = {
        hostPath = "/var/lib/minecraft";
        isReadOnly = false;
      };
    };
    forwardPorts = [ { containerPort = 25565; hostPort = 25565; protocol = "tcp"; } ];
  };

  networking.nat = {
    enable = true;
    internalInterfaces = ["ve-+"];
    # Apparently this can be left blank for outbound NAT?
    # externalInterface = "enp3s0f0";
  };

  environment.systemPackages = with pkgs; [
    mcrcon
    mbuffer
  ];

  services.tailscale.enable = true;
  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = 1;
    "net.ipv6.conf.all.forwarding" = 1;
  };
}
