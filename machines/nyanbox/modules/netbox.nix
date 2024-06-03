{ config, lib, pkgs, ... }:

let cfg = config.my.netbox; in

{
  options.my.netbox.enable = lib.mkEnableOption "netbox";

  config = lib.mkIf cfg.enable {

    services.oauth2-proxy = {
      enable = true;
      nginx.virtualHosts."netbox.zoiks.net" = {};
    };

    services.nginx = {
      upstreams.netbox-wsgi.servers = { "192.168.99.2:8001" = {}; };
      upstreams.netbox-static.servers = { "192.168.99.2:80" = {}; };
      virtualHosts."netbox.zoiks.net" = {
        enableACME = true;
        forceSSL = true;
        extraConfig = ''
          proxy_buffer_size 8k;
        '';

        locations."/" = {
          proxyPass = "http://netbox-wsgi/";
          proxyWebsockets = true;
          extraConfig = ''
            auth_request_set $username $upstream_http_x_auth_request_preferred_username;
            proxy_set_header X-Username $username;
          '';
        };

        locations."/static/" = {
          proxyPass = "http://netbox-static/static/";
        };
      };
    };

    containers.netbox = {
      autoStart = true;
      privateNetwork = true;
      timeoutStartSec = "5min";
      hostAddress = "192.168.99.1";
      localAddress = "192.168.99.2";
      config = { config, pkgs, ... }: {
        networking.nameservers = [ "192.168.7.1" ];
        networking.useHostResolvConf = false;
        system.stateVersion = "23.05";
        services.nginx = {
          enable = true;
          user = "netbox";
          # upstreams = {
          #   netbox.servers = { "127.0.0.1:8001" = {}; };
          # };
          # recommendedProxySettings = true;
          virtualHosts = {
            "netbox.zoiks.net" = {
              locations."/static/" = {
                alias = "/var/lib/netbox/static/";
              };
            };
          };
        };

        services.netbox = {
          enable = true;
          package = pkgs.netbox_3_7;
          secretKeyFile = "/var/lib/netbox/.secretkey.txt";
          listenAddress = "0.0.0.0";
          settings = {
            # BASE_PATH = "netbox/";
            REMOTE_AUTH_ENABLED = true;
            REMOTE_AUTH_HEADER = "HTTP_X_USERNAME";
            REMOTE_AUTH_USER_EMAIL = "HTTP_X_EMAIL";
          };
        };
        networking.firewall.allowedTCPPorts = [ 80 8001 ];

        systemd.services.netbox = {
          serviceConfig = {
            TimeoutStartSec = 600;
          };
        };
      };
    };

  };
}
