{ config, pkgs, ... }:

{
  nixpkgs.config.packageOverrides = super: rec {

    haskellPackages = super.haskellPackages.override {
      overrides = self: super: {
        taffybar-plugins = self.callPackage ../../pkgs/taffybar-plugins {};

        taffybar = self.callPackage ../../pkgs/taffybar {
          pkgs_gtk3 = pkgs.gtk3;
        };

        gtk-traymanager = self.callPackage ../../pkgs/gtk-traymanager {
          pkgs_gtk3 = pkgs.gtk3;
        };

      };
    };

    taffybar = super.taffybar.override (_: {
      packages = _: [haskellPackages.taffybar-plugins];
    });
  };
}
