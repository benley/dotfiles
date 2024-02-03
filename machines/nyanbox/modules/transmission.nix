{ config, lib, pkgs, ... }:

let
  cfg = config.my.transmission;
  secrets = import ../secrets.nix;
in

with lib;
{
  options.my.transmission = {
    enable = mkEnableOption "transmission";
    peer-port = mkOption {
      type = types.port;
      default = 54191;
    };
  };

  config = mkIf cfg.enable {

    networking.hosts = {
      # This is the stupidest workaround
      # See https://github.com/transmission/transmission/issues/407
      "87.98.162.88" = ["portcheck.transmissionbt.com"];
    };

    # TODO: does transmission actually use UDP???
    networking.firewall.allowedTCPPorts = [ config.my.transmission.peer-port ];
    networking.firewall.allowedUDPPorts = [ config.my.transmission.peer-port ];

    services.nginx = {
      upstreams.transmission.servers = { "127.0.0.1:9091" = {}; };

      virtualHosts."nyanbox.zoiks.net" = let rootExtraConfig = config.services.nginx.virtualHosts."nyanbox.zoiks.net".locations."/".extraConfig; in {
        locations."/transmission/" = {
          proxyPass = "http://transmission";  # no trailing /! I don't remember why.
          extraConfig = rootExtraConfig + ''
            proxy_pass_header X-Transmission-Session-Id;

            proxy_set_header X-NginX-Proxy true;
            proxy_http_version 1.1;
            proxy_set_header Connection "";
            proxy_pass_header X-Transmission-Session-Id;
            # add_header   Front-End-Https   on;

            location = /transmission/ {
              return 301 /transmission/web/;
            }
            location = /transmission/web {
              return 301 /transmission/web/;
            }
          '';
        };
      };
    };

    services.transmission = {
      enable = true;
      settings = {
        blocklist-enabled = true;
        blocklist-url = "https://list.iblocklist.com/?list=bt_level1&fileformat=p2p&archiveformat=gz";
        download-dir = "/z/downloads";
        encryption = 2;  # Require encryption
        incomplete-dir = "/z/downloads/incomplete";
        peer-port = config.my.transmission.peer-port;
        peer-port-random-on-start = false;
        port-forwarding-enabled = false;
        ratio-limit = "1";
        ratio-limit-enabled = true;
        rpc-bind-address = "127.0.0.1";
        rpc-host-whitelist-enabled = true;
        rpc-host-whitelist = "nyanbox.zoiks.net";
        speed-limit-up = 256;
        speed-limit-up-enabled = true;
      };
    };
  };
}
