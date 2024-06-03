{ config, lib, pkgs, ... }:

with lib;
let cfg = config.my.oauth2_proxy; in
{
  options.my.oauth2_proxy.enable = mkEnableOption "oauth2_proxy";

  config = mkIf cfg.enable {

    # It seems weird that I need to do this
    services.nginx.virtualHosts."nyanbox.zoiks.net".locations."/oauth2/".extraConfig = ''
      auth_request off;
    '';

    # TODO: consider separate proxy instances per service, in containers?
    services.oauth2-proxy = {
      enable = true;
      keyFile = "/var/lib/oauth2_proxy/oauth2_proxy.env";
      email.domains = [ "*" ];
      nginx = {
        virtualHosts."nyanbox.zoiks.net" = {};
        domain = "nyanbox.zoiks.net";
      };
      provider = "keycloak-oidc";
      clientID = "oauth2-proxy";
      # clientSecret = secrets.oauth2_proxy.clientSecret;    # in keyFile
      # cookie.secret = secrets.oauth2_proxy.cookie.secret;  # in keyFile
      setXauthrequest = true;
      extraConfig = {
        cookie-domain = ".zoiks.net";
        skip-provider-button = true;
        whitelist-domain = ".zoiks.net";
        set-authorization-header = true;
        oidc-issuer-url = "https://nyanbox.zoiks.net/auth/realms/master";
        #allowed-role = "foo";
        allowed-group = "/proxy";
        code-challenge-method = "S256";
        # This should be covered by setXauthrequest (look up a level)
        # set-xauthrequest = true;
        # I don't think anything uses this, probably safe to disable:
        pass-access-token = true;
      };
      scope = "openid email profile";
    };

    # https://gist.github.com/benley/78a5e84c52131f58d18319bf26d52cda
    systemd.services.oauth2-proxy = {
      # oauth2_proxy won't start until keycloak is running
      after = mkIf config.services.keycloak.enable ["keycloak.service"];
      wants = mkIf config.services.keycloak.enable ["keycloak.service"];
      # Don't give up trying to start oauth2_proxy, even if keycloak isn't up yet
      startLimitIntervalSec = 0;
      serviceConfig = {
        RestartSec = 1;
      };
    };
  };
}
