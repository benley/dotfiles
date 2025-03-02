{ config, lib, pkgs, ... }:

with lib;
let cfg = config.my.grafana; in
{
  options.my.grafana.enable = mkEnableOption "grafana";

  config = mkIf cfg.enable {

    services.nginx = {
      upstreams.grafana.servers = { "127.0.0.1:3000" = {}; };

      virtualHosts."nyanbox.zoiks.net" = {
        locations."/grafana/" = {
          proxyPass = "http://grafana/";
        };
      };
    };

    sops.secrets.grafana_oauth_client_secret = {
      owner = config.users.users.grafana.name;
      group = config.users.users.grafana.group;
    };

    services.grafana = {
      enable = true;
      settings = {
        auth = {
          disable_login_form = true;
          signout_redirect_url = "https://nyanbox.zoiks.net/auth/realms/master/protocol/openid-connect/logout";
        };
        "auth.generic_oauth" = {
          auto_login = true;
          enabled = true;
          name = "Keycloak";
          allow_sign_up = true;
          client_id = "grafana";
          client_secret = "$__file{${config.sops.secrets.grafana_oauth_client_secret.path}}";
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
