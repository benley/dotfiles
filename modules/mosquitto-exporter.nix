{ config, pkgs, lib, ... }:

let
  cfg = config.services.mosquitto-exporter;
  types = lib.types;
in

{
  options.services.mosquitto-exporter = {
    enable = lib.mkEnableOption "Prometheus Mosquitto Exporter";

    endpoint = lib.mkOption {
      type = types.str;
      default = "tcp://localhost:1883";
      description = ''
        Endpoint for the Mosquitto message broker
      '';
    };

    bind-address = lib.mkOption {
      type = types.str;
      default = "0.0.0.0:9234";
      description = ''
        Listen address for metrics HTTP endpoint
      '';
    };

    extraFlags = lib.mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        Extra commandline options to pass to the Mosquitto exporter.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.mosquitto-exporter = {
      description = "Prometheus Mosquitto exporter";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Restart = "always";
        ExecStart = ''
          ${pkgs.mosquitto-exporter}/bin/mosquitto-exporter \
            --endpoint ${cfg.endpoint} \
            --bind-address ${cfg.bind-address} \
            ${lib.concatStringsSep " \\\n  " cfg.extraFlags}
        '';
      };
    };
  };
}
