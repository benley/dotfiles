{ config, lib, pkgs, ... }:

let
  cfg = config.my.grafana;
  secrets = import ../secrets.nix;
in
{
  options.my.grafana.enable = lib.mkEnableOption "grafana";

  config = lib.mkIf cfg.enable {

    services.nginx = {
      upstreams.grafana.servers = { "127.0.0.1:3000" = {}; };

      virtualHosts."nyanbox.zoiks.net" = let rootExtraConfig = config.services.nginx.virtualHosts."nyanbox.zoiks.net".locations."/".extraConfig; in {
        locations."/grafana/" = {
          proxyPass = "http://grafana/";
          extraConfig = rootExtraConfig;
        };
      };
    };

    services.grafana = {
      enable = true;
      settings = {
        auth = {
          disable_login_form = true;
          signout_redirect_url = "https://nyanbox.zoiks.net/auth/realms/master/protocol/openid-connect/logout";
        };
        "auth" = {
          # Disable this after upgrading to grafana 10, I think?
          oauth_allow_insecure_email_lookup = true;
        };
        "auth.generic_oauth" = {
          auto_login = true;
          enabled = true;
          name = "Keycloak";
          allow_sign_up = true;
          client_id = secrets.grafana.oauth_client_id;
          client_secret = secrets.grafana.oauth_client_secret;
          scopes = "openid email profile offline_access roles";
          auth_url = "https://nyanbox.zoiks.net/auth/realms/master/protocol/openid-connect/auth";
          token_url = "https://nyanbox.zoiks.net/auth/realms/master/protocol/openid-connect/token";
          api_url = "https://nyanbox.zoiks.net/auth/realms/master/protocol/openid-connect/userinfo";
          role_attribute_path = "contains(roles[*], 'admin') && 'Admin' || contains(roles[*], 'editor') && 'Editor' || 'Viewer'";
          email_attribute_path = "email";
          login_attribute_path = "preferred_username";
          name_attribute_path = "name";
        };
        server.root_url = "https://nyanbox.zoiks.net/grafana";
      };
    };

  };
}
