{ config, pkgs, ... }:

{
  nixpkgs.config.packageOverrides = origPkgs: with origPkgs; {

    # This would enable kerberos in the default openssh package so it gets
    # included with things like git. Unfortunately, that causes nix to rebuild
    # a ton of stuff, which takes *forever*.
    #
    # openssh = openssh.override {
    #   withKerberos = true;
    #   withGssapiPatches = true;
    # };

  };
}
