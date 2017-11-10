{ config, pkgs, ... }:

{
  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in rec {

    haskellPackages = super.haskellPackages.override {
      overrides = self: super: {
        taffybar-plugins = self.callPackage ../../pkgs/taffybar-plugins {};
      };
    };

    taffybar = super.taffybar.override (_: {
      packages = _: [haskellPackages.taffybar-plugins];
    });

  };
}
