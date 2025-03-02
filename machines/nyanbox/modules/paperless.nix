{ config, lib, pkgs, ... }:

with lib;
let cfg = config.my.paperless; in
{
  options.my.paperless.enable = mkEnableOption "paperless-ngx";
  options.my.paperless.vhost = mkOption {
    type = types.str;
    default = "paperless.zoiks.net";
  };

  config = mkIf cfg.enable {

    users.users.paperless-scanner = {
      group = "paperless";
      isSystemUser = true;
    };

    services.nginx = {
      upstreams.paperless.servers = { "127.0.0.1:${toString config.services.paperless.port}" = {}; };

      virtualHosts.${cfg.vhost} = {
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
          '';
        };

        # Disable the django admin site, since it is a brute-force vector
        locations."/admin/" = {
          return = "403";
        };
      };
    };

    sops.secrets.paperless_oidc_client_secret = {};

    sops.templates."paperless.conf" = {
      owner = "paperless";
      content = lib.toShellVars {
        PAPERLESS_SOCIALACCOUNT_PROVIDERS = builtins.toJSON {
          openid_connect = {
            OAUTH_PKCE_ENABLED = true;
            APPS = [{
              provider_id = "keycloak";
              name = "Keycloak";
              client_id = "paperless";
              secret = config.sops.placeholder.paperless_oidc_client_secret;
              settings.server_url = "https://nyanbox.zoiks.net/auth/realms/master/.well-known/openid-configuration";
            }];
          };
        };
      };
    };

    services.paperless = {
      enable = true;
      dataDir = "/var/lib/paperless";
      settings = {
        PAPERLESS_CONFIGURATION_PATH = config.sops.templates."paperless.conf".path;
        PAPERLESS_APPS = "allauth.socialaccount.providers.openid_connect";
        PAPERLESS_DISABLE_REGULAR_LOGIN = true;
        PAPERLESS_REDIRECT_LOGIN_TO_SSO = true;

        # Hopefully tell OCR that dates are MM/DD/YYYY, not DD/MM/YYYY
        PAPERLESS_DATE_ORDER = "MDY";
        PAPERLESS_URL = "https://${cfg.vhost}";
        PAPERLESS_ALLOWED_HOSTS = "${cfg.vhost},nyanbox.zoiks.net,localhost";
        # PAPERLESS_SECRET_KEY moved to /var/lib/paperless/nixos-paperless-secret-key
        PAPERLESS_TRUSTED_PROXIES = "127.0.0.1,192.168.7.24";
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

    # systemd.services."paperless-task-queue".serviceConfig = {
    #   MemoryMax = "16G";
    #   CPUQuota = "200%";
    # };

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
