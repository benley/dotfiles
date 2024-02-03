{ config, lib, pkgs, ... }:

with lib;
let cfg = config.my.minecraft-server; in
{
  options.my.minecraft-server.enable = mkEnableOption "Minecraft server";

  config = mkIf cfg.enable; {

    environment.systemPackages = [ pkgs.mcrcon ];

    networking.firewall.allowedTCPPorts = [ 25565 ];

    containers.minecraft = {
      config = import ./minecraft-container.nix;
      autoStart = false;
      bindMounts = {
        "/var/lib/minecraft" = {
          hostPath = "/var/lib/minecraft";
          isReadOnly = false;
        };
      };
      forwardPorts = [ { containerPort = 25565; hostPort = 25565; protocol = "tcp"; } ];
    };

  };
}
