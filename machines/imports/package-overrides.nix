{ config, pkgs, ... }:

{
  nixpkgs.config.packageOverrides = super: rec {

    haskellPackages = super.haskellPackages.override {
      overrides = self: super: {
        taffybar-plugins = self.callPackage ../../pkgs/taffybar-plugins {};

        taffybar = self.callPackage ../../pkgs/taffybar {};
      };
    };

    taffybar = super.taffybar.override (_: {
      packages = _: [haskellPackages.taffybar-plugins];
    });

  };
}
