{ config, lib, pkgs, ... }:

let cfg = config.my.prometheus; in
{
  options.my.prometheus.enable = lib.mkEnableOption "prometheus";

  config = lib.mkIf cfg.enable {

    services.nginx = {
      upstreams.prometheus.servers = { "127.0.0.1:9090" = {}; };
      virtualHosts."nyanbox.zoiks.net" = {
        locations."/prometheus/" = {
          proxyPass = "http://prometheus/";
        };
      };
    };

    services.prometheus = {
      enable = true;
      # Can't set this to true while using external credential files
      checkConfig = "syntax-only";

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
              "192.168.7.173:9100" # ein
              # "pve0.zoiks.net:9100"
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
        ../prometheus-node-rules.yml
      ];
      extraFlags = [
        "--storage.tsdb.retention=30d"
        "--web.route-prefix=/"
      ];
      webExternalUrl = "https://nyanbox.zoiks.net/prometheus";
    };
  };
}
