{ config, pkgs, lib, ... }:

let secrets = import ./secrets.nix; in

{
  imports = [
    ./hardware-configuration.nix
    ../imports/defaults.nix
    ../imports/workstation.nix
  ];

  boot.loader.grub = {
    device = "/dev/sda";
    enable = true;
    version = 2;
  };

  boot.kernelModules = ["nct6775"]; # make /sys/class/hwmon/hwmon2 exist

  networking.hostName = "nyanbox";
  networking.hostId = "007f0101";

  # services.fancontrol.enable = true;
  # services.fancontrol.configFile = ./fancontrol.conf;

  services.hddfancontrol = {
    enable = true;
    disks = ["/dev/sdb" "/dev/sdc" "/dev/sdd" "/dev/sde"];
    pwm_paths = ["/sys/class/hwmon/hwmon0/pwm1"];
    use_smartctl = true;
    extra_args = lib.concatStringsSep " " [
      "--pwm-start-value 32"
      "--pwm-stop-value 0"
      "--spin-down-time 900"
      "--max-temp 45"
      "--cpu-sensor /sys/devices/platform/nct6775.656/hwmon/hwmon0/temp2_input"
      "--cpu-temp-range 30 75"
      # " -v debug";
    ];
  };

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
    };
  };

  services.openssh.enable = true;

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      1883 # mqtt
      8989 # WeMo device callback
      9090 # Prometheus
      3000 # Grafana
      80 443
      54191 # Transmission peering
      139 445 # Samba
      6052 # esphome dashboard
      25565 # minecraft
    ];
    allowedUDPPorts = [
      67 68 # bootp
      137 138 # Samba
      # 3084  # WeMo?
    ];
    allowedUDPPortRanges = [
      # Allow all the upnp nonsense that's impossible to handle correctly
      { from = 32767; to = 65535; }
    ];
  };

  programs.mosh.enable = true;  # this opens UDP 60000 to 61000 in the firewall

  system.stateVersion = "18.03";

  virtualisation.docker.enable = true;

  services.mosquitto = {
    enable = true;
    host = "0.0.0.0";  # non-ssl host
    users = {
      esphome = {
        acl = ["topic readwrite #"];
      };
      hass = {
        acl = ["topic readwrite #"];
      };
      prometheus = {
        acl = [
          "topic read #"
          "topic read $SYS/#"
        ];
      };
    };
  };

  services.mosquitto-exporter = {
    enable = true;
    extraFlags = ["--user prometheus"];
  };

  services.prometheus = {
    enable = true;
    globalConfig = {
      scrape_interval = "15s";
      evaluation_interval = "15s";
      scrape_timeout = "10s";
    };
    scrapeConfigs = [
      {
        job_name = "prometheus";
        static_configs = [{
          targets = ["localhost:9090"];
        }];
      }
      {
        job_name = "node";
        static_configs = [{
          targets = [
            "localhost:9100"
            "homeslice.zoiks.net:9100"
          ];
        }];
      }
      {
        job_name = "grafana";
        static_configs = [{
          targets = ["localhost:3000"];
        }];
      }
      {
        job_name = "hass";
        scrape_interval = "15s";
        metrics_path = "/api/prometheus";
        bearer_token = secrets.hass_bearer_token;
        static_configs = [{
          targets = ["homeslice.zoiks.net:8123"];
        }];
      }
      {
        job_name = "blackbox-http";
        metrics_path = "/probe";
        params = {
          module = ["http_2xx"];
        };
        static_configs = [{
          targets = [
            "http://google.com"
            "https://hass.zoiks.net/"
          ];
        }];
        relabel_configs = [
          {
            source_labels = ["__address__"];
            target_label = "__param_target";
          }
          {
            source_labels = ["__param_target"];
            target_label = "instance";
          }
          {
            target_label = "__address__";
            replacement = "nyanbox.zoiks.net:9115";
          }
        ];
      }
      {
        job_name = "blackbox-ping";
        metrics_path = "/probe";
        params = {
          module = ["icmp"];
        };
        static_configs = [{
          targets = [
            "hass.zoiks.net"
            "osric.zoiks.net"
            "8.8.8.8"
            "2001:4860:4860::8888"
          ];
        }];
        relabel_configs = [
          {
            source_labels = ["__address__"];
            target_label = "__param_target";
          }
          {
            source_labels = ["__param_target"];
            target_label = "instance";
          }
          {
            target_label = "__address__";
            replacement = "nyanbox.zoiks.net:9115";
          }
        ];
      }

      {
        job_name = "mosquitto";
        static_configs = [{
          targets = ["localhost:9234"];
        }];
      }
    ];
    ruleFiles = [ ./prometheus-node-rules.yml ];
    extraFlags = [
      "--storage.tsdb.retention=30d"
      "--web.route-prefix=/"
    ];
    webExternalUrl = "https://nyanbox.zoiks.net/prometheus";
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

  services.prometheus.exporters.blackbox = {
    enable = true;
    configFile = ./blackbox-exporter.yml;
  };

  services.cron.systemCronJobs = [
    ''
      * * * * * root mkdir -p /var/lib/node-exporter/textfile; cd /var/lib/node-exporter/textfile; PATH=${pkgs.smartmontools}/bin:$PATH ${./smartmon-textfile.sh} | ${pkgs.moreutils}/bin/sponge smartmon.prom
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
      AUTH_GOOGLE_ALLOW_SIGN_UP = "true";
      AUTH_GOOGLE_ALLOWED_DOMAINS = "zoiks.net";
    };
    rootUrl = "https://nyanbox.zoiks.net/grafana";
    #security.adminPassword = secrets.grafana.admin_password;
  };

  security.acme.email = "benley@zoiks.net";
  security.acme.acceptTerms = true;

  services.nginx = {
    enable = true;
    upstreams = {
      prometheus.servers = { "127.0.0.1:9090" = {}; };
      grafana.servers = { "127.0.0.1:3000" = {}; };
      transmission.servers = { "127.0.0.1:9091" = {}; };
      home-assistant.servers = { "homeslice.zoiks.net:8123" = {}; };
    };
    recommendedProxySettings = true;

    # TODO: in theory the stuff that oauth2_proxy puts in the root extraConfig
    # could actually go in the outer nginx "server" section, and then I
    # wouldn't have to hack it into each subpath like I'm doing here.  I think
    # it would just require adding "auth_request off" to the the location block
    # for /.well-known/acme-challenge to keep ACME stuff working.
    virtualHosts = {

      "hass.zoiks.net" = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://home-assistant";
        };
        locations."/api/websocket" = {
          proxyPass = "http://home-assistant/api/websocket";
          proxyWebsockets = true;
        };
      };

      "nyanbox.zoiks.net" = let rootExtraConfig = config.services.nginx.virtualHosts."nyanbox.zoiks.net".locations."/".extraConfig; in {
        default = true;  # this is the default vhost
        enableACME = true;
        forceSSL = true;

        locations."/" = {
          tryFiles = "$uri $uri/ =404";
        };

        locations."/prometheus/" = {
          proxyPass = "http://prometheus/";
          extraConfig = rootExtraConfig;
        };

        locations."/grafana/" = {
          proxyPass = "http://grafana/";
          extraConfig = rootExtraConfig;
        };

        locations."/transmission/" = {
          proxyPass = "http://transmission";  # no trailing /! I don't remember why.
          extraConfig = rootExtraConfig + ''
            proxy_pass_header X-Transmission-Session-Id;

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
      rpc-bind-address = "127.0.0.1";
      rpc-host-whitelist-enabled = true;
      rpc-host-whitelist = "nyanbox.zoiks.net";
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

    extraConfig = ''
      [homes]
      read only = no
      guest ok = no
    '';
  };

  system.autoUpgrade.enable = true;

  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 14d";

  services.oauth2_proxy = {
    enable = true;
    email.domains = [ "zoiks.net" ];
    nginx.virtualHosts = [ "nyanbox.zoiks.net" ];
    provider = "google";
    clientID = secrets.oauth2_proxy.clientID;
    clientSecret = secrets.oauth2_proxy.clientSecret;
    cookie.secret = secrets.oauth2_proxy.cookie.secret;
    setXauthrequest = true;
    extraConfig = {
      cookie-domain = ".zoiks.net";
      skip-provider-button = true;
      whitelist-domain = ".zoiks.net";
      set-authorization-header = true;
    };
    scope = "openid email profile";
  };

  services.udev.packages = [

    # I think this could go in 99-local.rules but I'm not entirely sure
    (pkgs.writeTextFile {
      name = "hubZ-udev-rules";
      text = builtins.readFile ./zigbee-zwave-udev.rules;
      destination = "/etc/udev/rules.d/70-hubZ.rules";
    })

  ];

  containers.minecraft = {
    config = import ./minecraft-container.nix;
    autoStart = true;
    bindMounts = {
      "/var/lib/minecraft" = {
        hostPath = "/var/lib/minecraft";
        isReadOnly = false;
      };
    };
    forwardPorts = [ { containerPort = 25565; hostPort = 25565; protocol = "tcp"; } ];
  };

  environment.systemPackages = with pkgs; [
    mcrcon
    mbuffer
  ];

}
