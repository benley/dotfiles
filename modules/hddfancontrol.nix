{ config, pkgs, lib, ... }:

let
  cfg = config.services.hddfancontrol;
  types = lib.types;
in

{
  options = {

    services.hddfancontrol.enable = lib.mkEnableOption "hddfancontrol daemon";

    services.hddfancontrol.disks = lib.mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        Drive(s) to get temperature from
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

    services.hddfancontrol.use_smartctl = lib.mkOption {
      type = types.bool;
      default = false;
      description = ''
        Probe temperature using smartctl instead of hddtemp or hdparm
      '';
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

  config = lib.mkIf cfg.enable (
    let args = lib.concatLists [
      ["-d"] cfg.disks
      ["-p"] cfg.pwm_paths
      (lib.optional cfg.use_smartctl "--smartctl")
    ]; in {

    systemd.packages = [pkgs.hddfancontrol];

    systemd.services.hddfancontrol = {
      enable = true;
      wantedBy = [ "multi-user.target" ];
      environment.HDDFANCONTROL_ARGS = "${lib.escapeShellArgs args} ${cfg.extra_args}";
    };
  });
}
