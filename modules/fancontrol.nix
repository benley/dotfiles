{ config, pkgs, lib, ... }:

let
  cfg = config.services.fancontrol;
  types = lib.types;
in

{
  options = {

    services.fancontrol.enable = lib.mkEnableOption "the fancontrol daemon";

    services.fancontrol.configFile = lib.mkOption {
      type = types.either types.path types.str;
      default = null;
      description = ''
        fancontrol config file contents
      '';
    };
  };

  config =
    let confFile = if lib.isString cfg.configFile
                   then pkgs.writeText "fancontrol" cfg.configFile
                   else cfg.configFile; in
    lib.mkIf (cfg.enable) {
    environment.systemPackages = [ pkgs.lm_sensors ];

    systemd.services.fancontrol = {
      description = "fancontrol daemon";
      wantedBy = [ "multi-user.target" ];
      script = ''
        exec ${pkgs.lm_sensors}/sbin/fancontrol ${confFile}
      '';
      serviceConfig = {
        Restart = "always";
      };
    };
  };
}
