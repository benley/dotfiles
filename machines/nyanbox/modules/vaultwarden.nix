{ config, lib, pkgs, ... }:

let cfg = config.my.vaultwarden; in

{
  options.my.vaultwarden.enable = lib.mkEnableOption "vaultwarden";

  config = lib.mkIf cfg.enable {

    services.nginx = {
      upstreams.vaultwarden.servers = { "127.0.0.1:8066" = {}; };

      virtualHosts."vault.zoiks.net" = {
        enableACME = true;
        forceSSL = true;

        locations."/" = {
          proxyPass = "http://vaultwarden/";
          proxyWebsockets = true;
        };
      };
    };

    # TODO: give vaultwarden its own zfs dataset
    # TODO: sqlite -> postgres/mysql?
    services.vaultwarden = {
      enable = true;
      backupDir = "/zfs/nyanbox/backup/vaultwarden";
      # dbBackend = "mysql";
      environmentFile = "/var/lib/bitwarden_rs/vaultwarden.env";
      config = {
        SIGNUPS_ALLOWED = false;
        ROCKET_PORT = 8066;
        DOMAIN = "https://vault.zoiks.net/";
        WEBSOCKET_ENABLED = true;

        SMTP_HOST = "smtp.sendgrid.net";
        SMTP_SECURITY = "starttls";
        SMTP_SSL = true;
        SMTP_FROM = "vaultwarden@zoiks.net";
        SMTP_FROM_NAME = "zoiks.net Vaultwarden server";
        SMTP_USERNAME = "apikey";
      };
    };

  };
}
