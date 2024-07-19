{ config, lib, pkgs, ... }:


let
  inherit (lib) mkEnableOption mkOption types;
  cfg = config.my.metube;
in
{
  options.my.metube = {
    enable = mkEnableOption "metube";

    port = mkOption {
      type = types.port;
      default = 18081;
      description = "http listen port for metube";
    };

    openFirewall = mkOption {
      type = types.bool;
      default = false;
    };

    listenAddress = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = "Interface address to listen on";
    };

    downloadsPath = mkOption {
      type = types.path;
      default = "/zfs/nyanbox/media/YouTube";
    };

    tempPath = mkOption {
      type = types.path;
      default = "/var/cache/metube";
    };
  };

  config = lib.mkIf cfg.enable {

    networking.firewall.allowedTCPPorts = lib.optional cfg.openFirewall cfg.port;

    users.users.metube = {
      uid = 990;
      isSystemUser = true;
      group = "metube";
      extraGroups = ["warez"];
    };
    users.groups.metube = {};

    services.nginx = {
      upstreams.metube.servers = { "127.0.0.1:${toString cfg.port}" = {}; };
      virtualHosts."nyanbox.zoiks.net" = {
        locations."/metube/" = {
          proxyPass = "http://metube/metube/";
          proxyWebsockets = true;
        };
      };
    };


    virtualisation.oci-containers.containers.metube = {
      image = "ghcr.io/alexta69/metube:2024-07-09";
      volumes = [
        "${cfg.downloadsPath}:/downloads"
        "${cfg.tempPath}:/var/cache/metube"
        "/root/ytdl-cookies.txt:/var/lib/ytdl/ytdl-cookies.txt"
      ];
      ports = ["${cfg.listenAddress}:${toString cfg.port}:8081/tcp"];
      environment = {
        UID = toString config.users.users.metube.uid;
        GID = toString config.users.groups.warez.gid;
        DOWNLOAD_DIR = "/downloads";
        TEMP_DIR = "/var/cache/metube";
        URL_PREFIX = "/metube/";
        # OUTPUT_TEMPLATE = "%(playlist_title&Playlist |)S%(playlist_title|)S%(playlist_uploader& by |)S%(playlist_uploader|)S%(playlist_autonumber& - |)S%(playlist_autonumber|)S%(playlist_count& of |)S%(playlist_count|)S%(playlist_autonumber& - |)S%(uploader,creator|UNKNOWN_AUTHOR)S - %(title|UNKNOWN_TITLE)S - %(release_date>%Y-%m-%d,upload_date>%Y-%m-%d|UNKNOWN_DATE)S.%(ext)s";
        # OUTPUT_TEMPLATE = "%(uploader)s/Season %(upload_date>%Y)s/s%(upload_date>%Y)s.e%(upload_date>%m%d)s01 - %(title)s.%(ext)s";
        OUTPUT_TEMPLATE = "%(uploader)s/Season %(upload_date>%Y)s/%(uploader)s - %(upload_date>%Y)s-%(upload_date>%m)s-%(upload_date>%d)s - %(title)s.%(ext)s";
        YTDL_OPTIONS = lib.strings.toJSON {
          cookiefile = "/var/lib/ytdl/ytdl-cookies.txt";
          writesubtitles = true;
          subtitleslangs = ["en" "-live_chat"];
          # Don't set file mtime to the youtube Last-modified header value
          updatetime = false;
          postprocessors = [
            # {
            #   "key" = "Exec";
            #   "exec_cmd" = "chmod 0664";
            #   "when" = "after_move";
            # }
            {
              "key" = "FFmpegEmbedSubtitle";
              "already_have_subtitle" = false;
            }
            {
              "key" = "FFmpegMetadata";
              "add_chapters" = true;
            }
          ];
        };
      };
    };
  };
}
