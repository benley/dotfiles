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

    plex = super.plex.overrideAttrs (x: rec {
      name = "plex-${version}";
      version = "1.13.2.5102";
      vsnHash = "4898d7d96";

      src = pkgs.fetchurl {
        url = "https://downloads.plex.tv/plex-media-server/${version}-${vsnHash}/plexmediaserver-${version}-${vsnHash}.x86_64.rpm";
        sha256 = "16mr9g6scv9ihqdq2lzaca8kbanr405mfsz0gip7cf65z1z4fjnj";
      };
    });

  };
}
