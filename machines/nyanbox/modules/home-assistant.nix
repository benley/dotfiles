{ config, lib, pkgs, ... }:

with lib;
let cfg = config.my.home-assistant; in
{
  options.my.home-assistant.enable = mkEnableOption "home-assistant stuff";

  config = mkIf cfg.enable {
    services.nginx = {
      upstreams.home-assistant.servers = { "192.168.7.36:8123" = {}; };
      virtualHosts."hass.zoiks.net" = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://home-assistant";
          proxyWebsockets = true;
        };
      };
    };

    services.samba.enable = true;
    services.samba.shares.hass_backup = {
      path = "/zfs/nyanbox/backup/hass";
      "read only" = false;
    };

    users.groups.hass = {};
    users.users.hass = {
      isSystemUser = true;
      group = "hass";
    };
  };
}
