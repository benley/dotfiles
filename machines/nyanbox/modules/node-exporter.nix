{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.my.node-exporter;
  node-exporter-textfile-collector-scripts = pkgs.fetchFromGitHub {
    owner = "prometheus-community";
    repo = "node-exporter-textfile-collector-scripts";
    rev = "34dd42ee2cf5bf1ffcfdea5a3599130f146b88fc";
    sha256 = "1vwhj6n4sh2ggigmhac8qzsai3mm020dpp5phwixvifi6jv57sid";
  };
in
{
  options.my.node-exporter.enable = mkEnableOption "node-exporter";

  config = mkIf cfg.enable {
    services.prometheus.exporters.node = {
      enable = true;
      extraFlags = [
        "--collector.textfile.directory=/var/lib/node-exporter/textfile"
        "--collector.filesystem.ignored-fs-types=^(sysfs|procfs|autofs|cgroup|devpts|nsfs|aufs|tmpfs|overlay|fuse|fuse\.lxc|mqueue)$"
        "--collector.filesystem.ignored-mount-points=^(/rootfs|/host)?/(sys|proc|dev|host|etc)($|/)"
      ];
      disabledCollectors = [ "timex" ];
      openFirewall = true;  # allow port 9100
    };

    services.cron.systemCronJobs = [
      ''
        * * * * * root mkdir -p /var/lib/node-exporter/textfile; cd /var/lib/node-exporter/textfile; PATH=${pkgs.smartmontools}/bin:$PATH ${../smartmon-textfile.sh} | ${pkgs.moreutils}/bin/sponge smartmon.prom
        * * * * * root mkdir -p /var/lib/node-exporter/textfile; ${pkgs.ipmitool}/bin/ipmitool sensor | ${pkgs.gawk}/bin/awk -f ${node-exporter-textfile-collector-scripts}/ipmitool | ${pkgs.moreutils}/bin/sponge /var/lib/node-exporter/textfile/ipmitool.prom
      ''
    ];
  };
}
