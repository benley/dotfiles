{ config, lib, pkgs, ... }:

let cfg = config.my.nyanbox-backups; in

{
  options.my.nyanbox-backups.enable = lib.mkEnableOption "nyanbox restic backups";

  config = lib.mkIf cfg.enable {
    services.restic.backups = {
      paperless = {
        backupPrepareCommand = ''
          DATASET=nyanbox/paperless
          SNAPSHOT="$DATASET@restic"
          # If the previous run's snapshot still exists, remove it
          zfs list -t snapshot "$SNAPSHOT" &>/dev/null && zfs destroy "$SNAPSHOT"
          zfs snapshot "$SNAPSHOT"
        '';
        backupCleanupCommand = ''
          DATASET=nyanbox/paperless
          SNAPSHOT="$DATASET@restic"
          zfs destroy "$SNAPSHOT"
        '';
        # credentials go here
        environmentFile = "...";
        passwordFile = "...";
        dynamicFilesFrom = ''
          # Script to collect the source snapshot paths goes here (maybe?)
          # n.b. you probably don't want the source paths to be changing on every run
          # so pointing at "whatever the latest snapshot is" in .zfs/snapshots
          # probably isn't the right approach
        '';
        paths = [
          "/var/lib/paperless/.zfs/snapshots/restic"
        ];
        repository = "...";
        timerconfig = {
          OnCalendar = "daily";
          Persistent = true;
        };
      };
    };
  };
}
