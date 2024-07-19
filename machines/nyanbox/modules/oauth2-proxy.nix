{ config, lib, pkgs, ... }:

with {
 cfg = config.my.oauth2-proxy;
 inherit (lib) mkEnableOption mkIf optional;

 redisServer = config.services.redis.servers.oauth2-proxy;
};

{
  options.my.oauth2-proxy = {
    enable = mkEnableOption "oauth2-proxy";
    enableRedis = mkEnableOption "redis for oauth2-proxy";
  };

  config = mkIf cfg.enable {

    services.redis.servers.oauth2-proxy.enable = mkIf cfg.enableRedis true;

    # TODO: consider separate proxy instances per service, in containers?
    services.oauth2-proxy = {
      enable = true;
      keyFile = "/var/lib/oauth2-proxy/oauth2-proxy.env";
      email.domains = [ "*" ];
      nginx = {
        virtualHosts."nyanbox.zoiks.net" = {};
        domain = "nyanbox.zoiks.net";
      };
      provider = "keycloak-oidc";
      clientID = "oauth2-proxy";
      # clientSecret = secrets.oauth2-proxy.clientSecret;    # in keyFile
      # cookie.secret = secrets.oauth2-proxy.cookie.secret;  # in keyFile
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
      } // lib.optionalAttrs cfg.enableRedis {
        session-store-type = "redis";
        redis-connection-url = "unix://${redisServer.unixSocket}";
      };
      scope = "openid email profile";
    };

    systemd.services.oauth2-proxy = {
      after = (optional cfg.enableRedis "redis-oauth2-proxy.service");
      # Don't give up trying to start oauth2-proxy, even if keycloak isn't up yet
      # https://gist.github.com/benley/78a5e84c52131f58d18319bf26d52cda
      startLimitIntervalSec = 0;
      serviceConfig = {
        RestartSec = 1;
        SupplementaryGroups = optional cfg.enableRedis redisServer.user;
      };
    };
  };
}
