{ config, pkgs, ... }:

{
  nixpkgs.config = {
    allowUnfree = true;
    chromium = {
      gnomeSupport = true;
      enablePepperFlash = true;
      enablePepperPDF = true;
      enableWideVine = true;
    };
  };

  nixpkgs.config.packageOverrides = origPkgs: with origPkgs; {

    # Enable kerberos in the default openssh package so it gets included with
    # things like git
    openssh = openssh.override {
      withKerberos = true;
      withGssapiPatches = true;
    };

  };
}
