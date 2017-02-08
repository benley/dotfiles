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

    # vim.gui = "gtk3";  # Not until vim 8, next release after nixos 16.09
  };

  nixpkgs.config.packageOverrides = origPkgs: with origPkgs; {

    # Enable kerberos in the default openssh package so it gets included with
    # things like git
    # ... except it causes a truly absurd number of rebuilds :-(
    #openssh = openssh.override {
    #  withKerberos = true;
    #  withGssapiPatches = true;
    #};

  };
}
