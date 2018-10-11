{ config, pkgs, lib, ... }:

with lib;
let cfg = config.services.powermate; in

{
  options.services.powermate = {
    enable = mkEnableOption "the powermate daemon";
  };

  config = mkIf cfg.enable {
    systemd.user.services.powermate = {
      description = "Powermate knob controller";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.powermate}/bin/powermate";
        RestartSec = "2000ms";
        Restart = "on-failure";
      };
    };

    services.udev.packages = [
      pkgs.powermate
    ];
  };
}
