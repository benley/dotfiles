{ config, lib, pkgs, ... }:

let cfg = config.my.keycloak; in

{
  options.my.keycloak.enable = lib.mkEnableOption "keycloak";

  config = lib.mkIf cfg.enable {
    services.nginx = {
      upstreams.keycloak.servers = { "127.0.0.1:8078" = {}; };

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
    };

    services.mysql.enable = true;
    services.mysql.package = pkgs.mariadb;

    # remove after upgrading to nixos 24.05
    nixpkgs.config.permittedInsecurePackages = [
      "keycloak-23.0.6"
    ];
  };
}
