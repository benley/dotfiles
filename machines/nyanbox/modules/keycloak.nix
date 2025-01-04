{ config, lib, pkgs, ... }:

let cfg = config.my.keycloak; in

{
  options.my.keycloak.enable = lib.mkEnableOption "keycloak";
  options.my.keycloak.port = lib.mkOption {
    default = 8078;
    description = "Backend http port for keycloak";
    type = lib.types.port;
  };

  config = lib.mkIf cfg.enable {

    # nginx outside the container
    services.nginx = {
      upstreams.keycloak.servers = { "${config.containers.keycloak.localAddress}:8078" = {}; };

      virtualHosts."nyanbox.zoiks.net" = {
        locations."/auth/" = {
          proxyPass = "http://keycloak/auth/";
          extraConfig = ''
            auth_request off;

            # Default is 4k which is too small for some auth responses
            proxy_buffer_size 16k;
            proxy_busy_buffers_size 32k;
            proxy_buffers 8 16k;
          '';
        };
      };
    };

    containers.keycloak = {
      autoStart = true;
      privateNetwork = true;
      hostAddress = "192.168.99.7";
      localAddress = "192.168.99.8";

      config = { config, pkgs, ... }: {
        networking.firewall.allowedTCPPorts = [ cfg.port ];
        networking.nameservers = [ "192.168.7.1" ];
        networking.useHostResolvConf = false;

        services.keycloak = {
          enable = true;
          database.type = "mariadb";
          database.createLocally = true;
          database.username = "keycloak";
          database.passwordFile = "/var/lib/mysql/.keycloak_db_passwd.txt";
          settings.hostname = "nyanbox.zoiks.net";
          settings.http-relative-path = "/auth";
          settings.http-port = cfg.port;
          settings.http-enabled = true;
          settings.proxy-headers = "xforwarded";
        };

        services.mysql.enable = true;
        services.mysql.package = pkgs.mariadb;
        system.stateVersion = "24.11";
      };
    };

  };
}
