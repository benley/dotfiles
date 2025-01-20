{ config, lib, pkgs, ... }:

let cfg = config.my.cadvisor; in
{
  options.my.cadvisor.enable = lib.mkEnableOption "cadvisor";
  options.my.cadvisor.enableMetrics = lib.mkOption {
    type = with lib.types; listOf str;
    description = "cadvisor metrics to explicitly enable";
    default = [
      "app"
      "cpu"
      "cpuLoad"
      "disk"
      "diskIO"
      "memory"
      "network"
      "oom_event"
      "percpu"
      "perf_event"
      "process"
    ];
  };

  config = lib.mkIf cfg.enable {
    services.cadvisor = {
      enable = true;
      port = 30141;
      extraOptions = [
        "-url_base_prefix=/cadvisor"
        "-enable_metrics=${lib.concatStringsSep "," cfg.enableMetrics}"
      ];
    };
    # add ps to cadvisor's path so process metrics work
    systemd.services.cadvisor.path = [pkgs.procps];

    services.nginx = {
      upstreams.cadvisor.servers = { "127.0.0.1:${toString config.services.cadvisor.port}" = {}; };
      virtualHosts."nyanbox.zoiks.net" = {
        locations."/cadvisor/" = {
          proxyPass = "http://cadvisor";
        };
      };
    };
  };
}
