{ config, pkgs, lib, ... }:

let cfg = config.services.hddfancontrol;
  types = lib.types;
in

{
  options = {

    services.hddfancontrol.enable = lib.mkEnableOption "hddfancontrol daemon";

    services.hddfancontrol.disks = lib.mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        List of devices to monitor
      '';
      example = ["/dev/sda"];
    };

    services.hddfancontrol.pwm_paths = lib.mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        PWM filepath(s) to control fan speed (under /sys)
      '';
      example = ["/sys/class/hwmon/hwmon2/pwm1"];
    };

    services.hddfancontrol.extra_args = lib.mkOption {
      type = types.str;
      default = "";
      description = ''
        Extra commandline arguments for hddfancontrol
      '';
      example = "--pwm-start-value 32 --pwm-stop-value 0 --spin-down-time 900";
    };
  };

  config = lib.mkIf cfg.enable {

    systemd.services.hddfancontrol = {
      description = "hddfancontrol daemon";
      wantedBy = [ "multi-user.target" ];
      script = ''
        exec ${pkgs.hddfancontrol}/bin/hddfancontrol \
          -d ${lib.concatStringsSep " " cfg.disks} \
          -p ${lib.concatStringsSep " " cfg.pwm_paths} \
          "$@"
        '';
      scriptArgs = cfg.extra_args;

      serviceConfig = {
        Restart = "always";
      };
    };

  };
}
