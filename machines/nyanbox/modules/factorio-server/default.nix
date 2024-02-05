{ config, lib, pkgs, ... }:

with lib;
let cfg = config.my.factorio-server; in
{
  options.my.factorio-server.enable = mkEnableOption "factorio server";

  config = mkIf cfg.enable {

    networking.firewall.allowedTCPPorts = [ 27015 ];
    networking.firewall.allowedUDPPorts = [ 27015 ];

    virtualisation.oci-containers.containers.factorio = {
      image = "factoriotools/factorio:1.1.76";
      volumes = ["/var/lib/factorio:/factorio"];
      ports = ["34197:34197/udp" "27015:27015/tcp"];
      environmentFiles = ["/var/lib/factorio/.factorio.env"];
      environment = {
        SAVE_NAME = "space exploration 0.6";
        LOAD_LATEST_SAVE = "false";
        USERNAME = "benley";
        # TOKEN = # moved to env file
        UPDATE_MODS_ON_START = "false";
      };
    };

  };
}
