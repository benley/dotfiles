{ config, lib, pkgs, ... }:

let
  cfg = config.my.oauth2_proxy;
  secrets = import ../secrets.nix;
in
with lib;
{
  options.my.oauth2_proxy.enable = mkEnableOption "oauth2_proxy";

  config = mkIf cfg.enable {

    # TODO: consider separate proxy instances per service, in containers?
    services.oauth2_proxy = {
      enable = true;
      email.domains = [ "*" ];
      nginx.virtualHosts = [ "nyanbox.zoiks.net" ];
      provider = "keycloak-oidc";
      clientID = secrets.oauth2_proxy.clientID;
      clientSecret = secrets.oauth2_proxy.clientSecret;
      cookie.secret = secrets.oauth2_proxy.cookie.secret;
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
    systemd.services.oauth2_proxy = {
      # oauth2_proxy won't start until keycloak is running
      after = mkIf services.keycloak.enable ["keycloak.service"];
      wants = mkIf services.keycloak.enable ["keycloak.service"];
      # Don't give up trying to start oauth2_proxy, even if keycloak isn't up yet
      startLimitIntervalSec = 0;
      serviceConfig = {
        RestartSec = 1;
      };
    };
  };
}
