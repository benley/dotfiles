{ config, pkgs, lib, ... }:

let secrets = import ./secrets.nix; in

{
  imports = [
    ./hardware-configuration.nix
    ../imports/defaults.nix
  ];

  boot.loader.grub = {
    device = "/dev/sda";
    enable = true;
    version = 2;
  };

  boot.kernelModules = ["nct6775"]; # make /sys/class/hwmon/hwmon2 exist

  networking.hostName = "nyanbox";
  networking.hostId = "007f0101";

  services.avahi.publish.enable = true;
  services.avahi.publish.workstation = true;
  services.avahi.publish.userServices = true;

  services.fancontrol.enable = true;
  services.fancontrol.configFile = ./fancontrol.conf;

  services.hddfancontrol = {
    enable = true;
    disks = ["/dev/sdb" "/dev/sdc" "/dev/sdd" "/dev/sde"];
    pwm_paths = ["/sys/class/hwmon/hwmon2/pwm1"];
    extra_args = "--pwm-start-value 32 --pwm-stop-value 0 --spin-down-time 900 -v debug";
  };

  services.openssh.enable = true;

  networking.firewall.allowedTCPPorts = [
    9090 # Prometheus
    3000 # Grafana
    80 443
    54191 # Transmission peering
    139 445 # Samba
  ];
  networking.firewall.allowedUDPPorts = [
    137 138 # Samba
  ];

  system.stateVersion = "18.03";

  virtualisation.docker.enable = true;

  systemd.services.prometheus = {
    description = "Prometheus";
    wantedBy = [ "multi-user.target" ];
    after = [ "docker.service" "docker.socket" ];
    requires = [ "docker.service" "docker.socket" ];
    script = ''
      exec ${pkgs.docker}/bin/docker run \
          --rm \
          --name=prometheus \
          --network=host \
          -v ${./prometheus.yml}:/etc/prometheus/prometheus.yml \
          -v prometheus_data:/prometheus \
          prom/prometheus:v2.2.1 \
          "$@"
    '';
    scriptArgs = lib.concatStringsSep " " [
      "--config.file=/etc/prometheus/prometheus.yml"
      "--storage.tsdb.path=/prometheus"
      "--web.console.libraries=/usr/share/prometheus/console_libraries"
      "--web.console.templates=/usr/share/prometheus/consoles"
      "--storage.tsdb.retention=30d"
      "--web.external-url=https://nyanbox.zoiks.net/prometheus"
      "--web.route-prefix=/"
    ];
    preStop = "${pkgs.docker}/bin/docker stop prometheus";
    reload = "${pkgs.docker}/bin/docker restart prometheus";
    serviceConfig = {
      ExecStartPre = "-${pkgs.docker}/bin/docker rm -f prometheus";
      ExecStopPost = "-${pkgs.docker}/bin/docker rm -f prometheus";
      TimeoutStartSec = 0;
      TimeoutStopSec = 120;
      Restart = "always";
    };
  };

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

  services.cron.systemCronJobs = [
    ''
      * * * * * root mkdir -p /var/lib/node-exporter/textfile; cd /var/lib/node-exporter/textfile; PATH=${pkgs.smartmontools}/bin:$PATH ${./smartmon-textfile.sh} > .smartmon.prom && mv .smartmon.prom smartmon.prom
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
      { device = "/dev/sde"; options = "-d sat"; }
    ];
  };

  services.grafana = {
    enable = true;
    extraOptions = {
      AUTH_GOOGLE_CLIENT_ID = secrets.grafana.google_client_id;
      AUTH_GOOGLE_CLIENT_SECRET = secrets.grafana.google_client_secret;
      AUTH_DISABLE_LOGIN_FORM = "true";
      AUTH_GOOGLE_ENABLED = "true";
      AUTH_GOOGLE_ALLOW_SIGN_UP = "false";
    };
    rootUrl = "https://nyanbox.zoiks.net/grafana";
    security.adminPassword = secrets.grafana.admin_password;
  };

  services.nginx = {
    enable = true;
    upstreams = {
      prometheus.servers = { "127.0.0.1:9090" = {}; };
      grafana.servers = { "127.0.0.1:3000" = {}; };
      transmission.servers = { "127.0.0.1:9091" = {}; };
    };

    virtualHosts = {
      "nyanbox.zoiks.net" = {
        enableACME = true;
        forceSSL = true;

        locations."/" = {
          tryFiles = "$uri $uri/ =404";
          extraConfig = ''
            allow 192.168.7.0/24;
            allow 71.192.172.48/32;
          '';
        };

        locations."/prometheus/" = {
          proxyPass = "http://prometheus/";
          extraConfig = ''
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-Host $host;
            proxy_set_header X-Forwarded-Server $host;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            allow 192.168.7.0/24;
            allow 71.192.172.48/32;
            deny all;
          '';
        };

        locations."/grafana/" = {
          proxyPass = "http://grafana/";
          extraConfig = ''
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-Host $host;
            proxy_set_header X-Forwarded-Server $host;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          '';
          # Allow all here since it's authenticated
        };

        locations."/transmission/" = {
          proxyPass = "http://transmission";  # no trailing /! I don't remember why.
          extraConfig = ''
            allow 192.168.7.0/24;
            allow 71.192.172.48/32;
            deny all;

            proxy_pass_header X-Transmission-Session-Id;

            proxy_set_header Host $http_host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Server $host;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;

            proxy_set_header X-NginX-Proxy true;
            proxy_http_version 1.1;
            proxy_set_header Connection "";
            proxy_pass_header X-Transmission-Session-Id;
            # add_header   Front-End-Https   on;

            location = /transmission/ {
              return 301 /transmission/web/;
            }
            location = /transmission/web {
              return 301 /transmission/web/;
            }
          '';
        };
      };
    };
  };

  services.transmission = {
    enable = true;
    settings = {
      blocklist-enabled = true;
      blocklist-url = "https://list.iblocklist.com/?list=bt_level1&fileformat=p2p&archiveformat=gz";
      download-dir = "/z/downloads";
      incomplete-dir = "/z/downloads/incomplete";
      peer-port = 54191;
      peer-port-random-on-start = false;
      ratio-limit = "0.0010";
      ratio-limit-enabled = true;
      rpc-host-whitelist-enabled = false;
      rpc-whitelist = "127.0.0.1,192.168.7.*";
      rpc-whitelist-enabled = false;
      speed-limit-up = 40;
      speed-limit-up-enabled = true;
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

    shares.downloads = {
      path = "/zfs/nyanbox/downloads";
    };
  };

  system.autoUpgrade = {
    channel = "https://nixos.org/channels/nixos-18.09";
    enable = true;
  };

  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 14d";
}
