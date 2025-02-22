{ config, lib, pkgs, ... }:

with lib;
let cfg = config.my.paperless; in
{
  options.my.paperless.enable = mkEnableOption "paperless-ngx";

  config = mkIf cfg.enable {

    users.users.paperless-scanner = {
      group = "paperless";
      isSystemUser = true;
    };

    services.oauth2-proxy = {
      enable = true;
      nginx.virtualHosts."paperless.zoiks.net" = {};
    };

    services.nginx = {
      upstreams.paperless.servers = { "127.0.0.1:${toString config.services.paperless.port}" = {}; };

      virtualHosts."paperless.zoiks.net" = {
        enableACME = true;
        forceSSL = true;

        extraConfig = ''
          proxy_buffer_size 8k;
          client_max_body_size 100M;
        '';

        locations."/" = {
          proxyPass = "http://paperless/";
          proxyWebsockets = true;
          extraConfig = ''
            # according to https://github.com/paperless-ngx/paperless-ngx/wiki/Using-a-Reverse-Proxy-with-Paperless-ngx#nginx
            proxy_redirect off;
            add_header Referrer-Policy "strict-origin-when-cross-origin";
            proxy_set_header _oauth2_proxy_0 "";

            auth_request_set $username $upstream_http_x_auth_request_preferred_username;
            proxy_set_header X-Username $username;
          '';
        };
      };
    };

    services.paperless = {
      enable = true;
      dataDir = "/var/lib/paperless";
      settings = {
        # Hopefully tell OCR that dates are MM/DD/YYYY, not DD/MM/YYYY
        PAPERLESS_DATE_ORDER = "MDY";
        PAPERLESS_URL = "https://paperless.zoiks.net";
        PAPERLESS_ALLOWED_HOSTS = "paperless.zoiks.net,nyanbox.zoiks.net,localhost";
        # PAPERLESS_SECRET_KEY moved to /var/lib/paperless/nixos-paperless-secret-key
        PAPERLESS_TRUSTED_PROXIES = "127.0.0.1,192.168.7.24";
        PAPERLESS_ENABLE_HTTP_REMOTE_USER = true;
        PAPERLESS_HTTP_REMOTE_USER_HEADER_NAME = "HTTP_X_USERNAME";
        PAPERLESS_USE_X_FORWARD_HOST = true;
        # PAPERLESS_USE_X_FORWARD_PORT = "true";
        PAPERLESS_FILENAME_FORMAT = "{{created_year}}/{{correspondent}}/{{title}}";
        # deskew seems to make things worse more often than it helps
        PAPERLESS_OCR_DESKEW = false;
        # Be more conservative about deciding to rotate pages (default is 12)
        PAPERLESS_OCR_ROTATE_PAGES_THRESHOLD = 15;
        PAPERLESS_OCR_SKIP_ARCHIVE_FILE = "with_text";
        PAPERLESS_OCR_USER_ARGS = builtins.toJSON {
          # I don't care about preserving signatures in OCR'd copies of documents
          invalidate_digital_signatures = true;
        };
        # Fix worker timeout at startup, somehow?
        # https://github.com/paperless-ngx/paperless-ngx/pull/1500 ???
        PAPERLESS_WEBSERVER_WORKERS = 2;
        # https://github.com/NixOS/nixpkgs/issues/240591
        LD_LIBRARY_PATH="${getLib pkgs.mkl}/lib";
      };
    };

    services.samba = {
      enable = true;
      settings.paperless_consume = {
        path = "/var/lib/paperless/consume";
        "read only" = false;
        "force create mode" = 660;
        "force directory mode" = 770;
      };
    };
  };
}
