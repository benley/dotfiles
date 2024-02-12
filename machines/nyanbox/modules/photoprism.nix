{ config, lib, pkgs, ... }:

with lib;
let cfg = config.my.photoprism; in
{
  options.my.photoprism = {
    enable = mkEnableOption "photoprism";
  };

  # This config is NOT IN USE YET and HAS NOT BEEN TESTED

  config = mkIf cfg.enable {
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
  };
}
