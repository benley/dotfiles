{ config, lib, pkgs, ... }:

let cfg = config.my.immich; in
{
  options.my.immich.enable = lib.mkEnableOption "immich";

  config = lib.mkIf cfg.enable {
    containers.immich = {
      autoStart = true;
      privateNetwork = true;
      hostAddress = "192.168.99.5";
      localAddress = "192.168.99.6";
      bindMounts = {
        "/var/lib/immich" = {
          hostPath = "/zfs/nyanbox/immich";
          isReadOnly = false;
        };
      };
      config = let hostConfig = config; in { config, pkgs, ... }: {
        networking.nameservers = [ "192.168.7.1" ];
        networking.useHostResolvConf = false;
        system.stateVersion = "24.11";

        services.immich = {
          enable = true;
          mediaLocation = "/var/lib/immich";
          openFirewall = true;
          host = "0.0.0.0";  # within the container
        };

        users.users.immich.uid = hostConfig.users.users.immich.uid;
        users.groups.immich.gid = hostConfig.users.groups.immich.gid;
      };
    };

    users.users.immich.uid = 988;
    users.users.immich.group = "immich";
    users.groups.immich.gid = 988;

    services.nginx = {
      upstreams.immich.servers = { "192.168.99.6:2283" = {}; };
      virtualHosts."immich.zoiks.net" = {
        enableACME= true;
        forceSSL = true;
        extraConfig = ''
          # Allow huge uploads
          client_max_body_size 50000M;
          proxy_read_timeout 600s;
          proxy_send_timeout 600s;
          send_timeout 600s;
        '';
        locations."/" = {
          proxyPass = "http://immich/";
          proxyWebsockets = true;
        };
      };
    };
  };

}
