{ config, lib, pkgs, ... }:

with lib;
let cfg = config.my.nextcloud; in
{
  options.my.nextcloud.enable = mkEnableOption "nextcloud";

  config = mkIf cfg.enable {

    services.nginx = {
      upstreams.nextcloud.servers = { "192.168.7.181:9001" = {}; };

      virtualHosts."nextcloud.zoiks.net" = {
        enableACME = true;
        forceSSL = true;

        extraConfig = ''
          proxy_buffer_size 8k;
        '';

        locations."/" = {
          proxyPass = "http://nextcloud/";
          proxyWebsockets = true;
          extraConfig = ''
          '';
        };

        locations."/.well-known/carddav" = {
          return = "301 $scheme://$host/remote.php/dav";
        };

        locations."/.well-known/caldav" = {
          return = "301 $scheme://$host/remote.php/dav";
        };

      };
    };
  };
}
