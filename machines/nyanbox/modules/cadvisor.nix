{ config, lib, pkgs, ... }:

let cfg = config.my.cadvisor; in
{
  options.my.cadvisor.enable = lib.mkEnableOption "cadvisor";

  config = lib.mkIf cfg.enable {
    services.cadvisor = {
      enable = true;
      port = 30141;
      extraOptions = ["-url_base_prefix=/cadvisor"];
    };

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
