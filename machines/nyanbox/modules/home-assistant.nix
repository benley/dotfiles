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
    services.samba.settings.hass_backup = {
      path = "/zfs/nyanbox/backup/hass";
      "read only" = false;
    };

    users.groups.hass = {};
    users.users.hass = {
      isSystemUser = true;
      group = "hass";
    };

    services.prometheus = {
      rules = [
        (builtins.toJSON {
          groups = [{
            name = "hass";
            rules = [{
              record = "entity:hass_sensor_energy_kwh:avg_over_time_10m";
              expr = "avg_over_time(hass_sensor_energy_kwh[10m])";
            }];
          }];
        })
      ];
      scrapeConfigs = [{
        job_name = "hass";
        scrape_interval = "15s";
        metrics_path = "/api/prometheus";
        bearer_token_file = "/var/lib/prometheus2/.hass_bearer_token";
        static_configs = [{
          targets = ["192.168.7.36:8123"];
        }];
      }];
    };
  };
}
