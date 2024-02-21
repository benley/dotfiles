{ config, lib, pkgs, ... }:

let cfg = config.my.vaultwarden; in

{
  options.my.vaultwarden.enable = lib.mkEnableOption "vaultwarden";

  config = lib.mkIf cfg.enable {

    services.nginx = {
      upstreams.vaultwarden.servers = { "127.0.0.1:8066" = {}; };
      upstreams.vaultwarden-ws.servers = { "127.0.0.1:3012" = {}; };

      virtualHosts."nyanbox.zoiks.net" = let rootExtraConfig = config.services.nginx.virtualHosts."nyanbox.zoiks.net".locations."/".extraConfig; in {
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

    # TODO: give vaultwarden its own zfs dataset
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
