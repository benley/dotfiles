{ config, pkgs, lib, ... }:

let secrets = import ./secrets.nix; in

with rec {
  quarkus-systemd-notify = pkgs.fetchMavenArtifact {
    groupId = "io.quarkiverse.systemd.notify";
    artifactId = "quarkus-systemd-notify";
    version = "1.0.1";
    hash = "sha256-3I4j22jyIpokU4kdobkt6cDsALtxYFclA+DV+BqtmLY=";
  };

  quarkus-systemd-notify-deployment = pkgs.fetchMavenArtifact {
    groupId = "io.quarkiverse.systemd.notify";
    artifactId = "quarkus-systemd-notify-deployment";
    version = "1.0.1";
    hash = "sha256-xHxzBxriSd/OU8gEcDG00VRkJYPYJDfAfPh/FkQe+zg=";
  };

  keycloak-plugins = pkgs.runCommand "keycloak-plugins" {} ''
      mkdir -p $out
      cp ${quarkus-systemd-notify}/share/java/*.jar $out/
      cp ${quarkus-systemd-notify-deployment}/share/java/*.jar $out/
    '';

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
    ./modules/nyanbox-backups.nix
    ./modules/netbox.nix
  ];

  my.netbox.enable = true;

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

  networking.hosts = {
    # This is the stupidest workaround
    # See https://github.com/transmission/transmission/issues/407
    "87.98.162.88" = ["portcheck.transmissionbt.com"];
  };

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
      1883 # mqtt
      9234 # mosquitto-exporter
      9090 # Prometheus
      80 443
      54191 # Transmission peering
      139 445 # Samba
      25565 # minecraft
      27015 # factorio
    ];
    allowedUDPPorts = [
      67 68 # bootp
      137 138 # Samba
      34197 # Factorio
      54191 # Transmission (does it actually use UDP here???)
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

  virtualisation.oci-containers.containers.hyperion = {
    image = "hyperion:2.0.12";
    volumes = ["hyperion-data:/var/lib/hyperion"];
    ports = ["8090:8090" "8092:8092" "19444:19444" "19400:19400" "19445:19445"];
    cmd = ["hyperiond" "-u" "/var/lib/hyperion" "-v"];
  };

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
            "192.168.7.24:9100"  # nyanbox
            "192.168.7.36:9100"  # ditto / homeassistant
            "pve0.zoiks.net:9100"
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
          targets = ["192.168.7.36:8123"];
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
    ];
    ruleFiles = [
      ./prometheus-node-rules.yml
      ./hass-rules.yml
    ];
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

  # services.code-server.enable = true;

  services.grafana = {
    enable = true;
    settings = {
      auth = {
        disable_login_form = true;
        signout_redirect_url = "https://nyanbox.zoiks.net/auth/realms/master/protocol/openid-connect/logout?redirect_uri=https%%3A%%2F%%2Fnyanbox.zoiks.net%%2Fgrafana";
      };
      "auth" = {
        # Disable this after upgrading to grafana 10, I think?
        oauth_allow_insecure_email_lookup = true;
      };
      "auth.generic_oauth" = {
        auto_login = true;
        enabled = true;
        name = "Keycloak";
        allow_sign_up = true;
        client_id = secrets.grafana.oauth_client_id;
        client_secret = secrets.grafana.oauth_client_secret;
        scopes = "openid email profile offline_access roles";
        auth_url = "https://nyanbox.zoiks.net/auth/realms/master/protocol/openid-connect/auth";
        token_url = "https://nyanbox.zoiks.net/auth/realms/master/protocol/openid-connect/token";
        api_url = "https://nyanbox.zoiks.net/auth/realms/master/protocol/openid-connect/userinfo";
        role_attribute_path = "contains(roles[*], 'admin') && 'Admin' || contains(roles[*], 'editor') && 'Editor' || 'Viewer'";
        email_attribute_path = "email";
        login_attribute_path = "preferred_username";
        name_attribute_path = "name";
      };
      server.root_url = "https://nyanbox.zoiks.net/grafana";
    };
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
      prometheus.servers = { "127.0.0.1:9090" = {}; };
      grafana.servers = { "127.0.0.1:3000" = {}; };
      transmission.servers = { "127.0.0.1:9091" = {}; };
      home-assistant.servers = { "192.168.7.36:8123" = {}; };
      nextcloud.servers = { "192.168.7.181:9001" = {}; };
      paperless.servers = { "127.0.0.1:${toString config.services.paperless.port}" = {}; };
      keycloak.servers = { "127.0.0.1:8078" = {}; };
      vaultwarden.servers = { "127.0.0.1:8066" = {}; };
      vaultwarden-ws.servers = { "127.0.0.1:3012" = {}; };
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
          proxyWebsockets = true;
        };
      };

      "paperless.zoiks.net" = {
        enableACME = true;
        forceSSL = true;

        extraConfig = ''
          proxy_buffer_size 8k;
          client_max_body_size 100M;
        '';

        locations."/" = {
          proxyPass = "http://paperless/";
          proxyWebsockets = true;
          extraConfig = ''
            # according to https://github.com/paperless-ngx/paperless-ngx/wiki/Using-a-Reverse-Proxy-with-Paperless-ngx#nginx
            proxy_redirect off;
            add_header Referrer-Policy "strict-origin-when-cross-origin";
            proxy_set_header _oauth2_proxy_0 "";

            auth_request_set $username $upstream_http_x_auth_request_preferred_username;
            proxy_set_header X-Username $username;
          '';
        };
      };

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

        locations."/auth/" = {
          proxyPass = "http://keycloak/auth/";
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

        locations."/vault/" = {
          proxyPass = "http://vaultwarden/vault/";
          proxyWebsockets = true;
        };
        locations."/vault/admin" = {
          proxyPass = "http://vaultwarden/vault/admin";
          proxyWebsockets = true;
          extraConfig = rootExtraConfig;
        };
        locations."/vault/notifications/hub/negotiate" = {
          proxyPass = "http://vaultwarden-ws/vault/";
          proxyWebsockets = true;
        };
        locations."/vault/notifications/hub" = {
          proxyPass = "http://vaultwarden-ws/vault/";
          proxyWebsockets = true;
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
      encryption = 2;  # Require encryption
      incomplete-dir = "/z/downloads/incomplete";
      peer-port = 54191;
      peer-port-random-on-start = false;
      port-forwarding-enabled = false;
      ratio-limit = "1";
      ratio-limit-enabled = true;
      rpc-bind-address = "127.0.0.1";
      rpc-host-whitelist-enabled = true;
      rpc-host-whitelist = "nyanbox.zoiks.net";
      speed-limit-up = 256;
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

    shares.hass_backup = {
      path = "/zfs/nyanbox/backup/hass";
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

  # TODO: consider separate proxy instances per service, in containers?
  services.oauth2_proxy = {
    enable = true;
    email.domains = [ "*" ];
    nginx.virtualHosts = [ "nyanbox.zoiks.net" "paperless.zoiks.net" ];
    provider = "keycloak-oidc";
    clientID = secrets.oauth2_proxy.clientID;
    clientSecret = secrets.oauth2_proxy.clientSecret;
    cookie.secret = secrets.oauth2_proxy.cookie.secret;
    setXauthrequest = true;
    extraConfig = {
      cookie-domain = ".zoiks.net";
      skip-provider-button = true;
      whitelist-domain = ".zoiks.net";
      set-authorization-header = true;
      oidc-issuer-url = "https://nyanbox.zoiks.net/auth/realms/master";
      #allowed-role = "foo";
      allowed-group = "/proxy";
      code-challenge-method = "S256";
      # This should be covered by setXauthrequest (look up a level)
      # set-xauthrequest = true;
      # I don't think anything uses this, probably safe to disable:
      pass-access-token = true;
    };
    scope = "openid email profile";
  };

  systemd.services.keycloak = {
    serviceConfig = {
      Type = "notify";
      NotifyAccess = "all";
    };
  };

  # https://gist.github.com/Clownfused/1144a4547fc428f7f690cd81b912ac74
  systemd.services.oauth2_proxy = {
    # oauth2_proxy won't start until keycloak is running
    after = ["keycloak.service"];
    wants = ["keycloak.service"];
    # Don't give up trying to start oauth2_proxy, even if keycloak isn't up yet
    startLimitIntervalSec = 0;
    serviceConfig = {
      RestartSec = 1;
    };
  };

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

  containers.photoprism = {
    autoStart = false;
    privateNetwork = true;
    hostAddress = "192.168.99.3";
    localAddress = "192.168.99.4";
    bindMounts = {
      "/data/photos" = {
        hostPath = "/zfs/nyanbox/photos";
        isReadOnly = false;
      };
    };
    config = { config, pkgs, ... }: {
      system.stateVersion = "23.11";
      services.mysql = {
        enable = true;
        dataDir = "/var/lib/mysql";
        package = pkgs.mariadb;
        ensureDatabases = [ "photoprism" ];
        ensureUsers = [{
          name = "photoprism";
          ensurePermissions = {
            "photoprism.*" = "ALL PRIVILEGES";
          };
        }];
      };
      services.photoprism = {
        enable = true;
        address = "0.0.0.0";
        port = 2342;
        originalsPath = "/data/photos";
        importPath = "import";  # relative to originalsPath
        passwordFile = "/var/lib/photoprism/._admin_password.txt";
        storagePath = "/var/lib/photoprism";
        settings = {
          PHOTOPRISM_DATABASE_DRIVER = "mysql";
          PHOTOPRISM_DATABASE_NAME = "photoprism";
          PHOTOPRISM_DATABASE_SERVER = "/run/mysqld/mysqld.sock";
          PHOTOPRISM_DATABASE_USER = "photoprism";
          PHOTOPRISM_SITE_URL = "https://photoprism.zoiks.net";
          # PHOTOPRISM_SITE_TITLE = "My PhotoPrism";
        };
      };
    };
  };

  environment.systemPackages = with pkgs; [
    mcrcon
    mbuffer
  ];

  # services.factorio = {
  #   enable = true;
  #   mods = [];
  #   saveName = "IndustrialRevolution";
  #   game-password = "...";
  # };

  # services.kubernetes = {
  #   roles = [ "master" "node" ];
  #   masterAddress = "nyanbox";
  #   kubelet.extraOpts = "--fail-swap-on=false";
  #   apiserver.allowPrivileged = true;
  #   apiserver.serviceClusterIpRange = "10.2.2.0/23";
  #   controllerManager.extraOpts = "--terminated-pod-gc-threshold=200";
  # };

  services.tailscale.enable = true;
  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = 1;
    "net.ipv6.conf.all.forwarding" = 1;
  };

  # TODO: put keycloak and its mysql instance into a nixos container
  services.keycloak = {
    enable = true;
    database.type = "mariadb";
    database.createLocally = true;
    database.username = "keycloak";
    database.passwordFile = "/var/lib/mysql/.keycloak_db_passwd.txt";
    settings.hostname = "nyanbox.zoiks.net";
    settings.http-relative-path = "/auth";
    settings.http-port = 8078;
    settings.http-enabled = true;
    settings.proxy = "reencrypt";
    plugins = [ keycloak-plugins ];
  };

  services.mysql.enable = true;  # for keycloak
  services.mysql.package = pkgs.mariadb;
  
  users.users.paperless-scanner = {
    group = "paperless";
    isSystemUser = true;
  };

  users.groups.hass = {};
  users.users.hass = {
    isSystemUser = true;
    group = "hass";
  };

  # TODO: maybe give vaultwarden its own zfs dataset?
  # TODO: sqlite -> postgres/mysql?
  services.vaultwarden = {
    enable = true;
    # dbBackend = "mysql";
    environmentFile = "/var/lib/bitwarden_rs/vaultwarden.env";
    config = {
      SIGNUPS_ALLOWED = false;
      ROCKET_PORT = 8066;
      DOMAIN = "https://nyanbox.zoiks.net/vault";
      WEBSOCKET_ENABLED = true;
    };
  };

  services.paperless = {
    enable = true;
    dataDir = "/var/lib/paperless";
    extraConfig = {
      # Hopefully tell OCR that dates are MM/DD/YYYY, not DD/MM/YYYY
      PAPERLESS_DATE_ORDER = "MDY";
      PAPERLESS_URL = "https://paperless.zoiks.net";
      PAPERLESS_ALLOWED_HOSTS = "paperless.zoiks.net,nyanbox.zoiks.net,localhost";
      # PAPERLESS_CORS_ALLOWED_HOSTS = "https://paperless.zoiks.net,https://nyanbox.zoiks.net";
      PAPERLESS_SECRET_KEY = secrets.paperless.secret_key;
      PAPERLESS_TRUSTED_PROXIES = "127.0.0.1,192.168.7.24";
      PAPERLESS_ENABLE_HTTP_REMOTE_USER = true;
      PAPERLESS_HTTP_REMOTE_USER_HEADER_NAME = "HTTP_X_USERNAME";
      PAPERLESS_USE_X_FORWARD_HOST = true;
      # PAPERLESS_USE_X_FORWARD_PORT = "true";
      PAPERLESS_FILENAME_FORMAT = "{created_year}/{correspondent}/{title}";
      PAPERLESS_OCR_USER_ARGS = builtins.toJSON {
        # I don't care about preserving signatures in OCR'd copies of documents
        invalidate_digital_signatures = true;
      };
      # https://github.com/NixOS/nixpkgs/issues/240591
      LD_LIBRARY_PATH="${lib.getLib pkgs.mkl}/lib";
    };
  };
}
