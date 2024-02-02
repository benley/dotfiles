{ config, lib, pkgs, ... }:

let cfg = config.my.netbox; in

{
  options.my.netbox.enable = lib.mkEnableOption "netbox";

  config = lib.mkIf cfg.enable {
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
          package = pkgs.netbox_3_6;
          secretKeyFile = ../netbox-secret-key.txt;
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
