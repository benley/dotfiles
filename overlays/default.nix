self: super:

with { callPackage = super.callPackage; };

{
  awsudo = super.pythonPackages.callPackage ../pkgs/awsudo { };

  eless = super.writeScriptBin "eless" (builtins.readFile ../eless.sh);

  kubernetes-client = callPackage ../pkgs/kubernetes-client { };

  nix-home = callPackage ../pkgs/nix-home { };

  nixhomeLib = callPackage (import "${nix-home}/nix/lib/nixhome") {};

  hddfancontrol = callPackage ../pkgs/hddfancontrol { };

  homedir = callPackage ../homedir.nix { mkHome = nixhomeLib.mkHome; };

  # Based on stuff from:
  #  https://beyermatthias.de/blog/2015/11/25/how-to-setup-neovim-on-nixos/
  #  http://nerditya.com/code/guide-to-neovim/
  # vim = super.lib.overrideDerivation (
  #   super.vim_configurable.customize {
  #     name = "vim";
  #     vimrcConfig = (import ./vim/customization.nix { pkgs = super; });
  #   }
  # ) (_: { ftNixSupport = false; });

  # neovim = self.neovim.override {
  #   vimAlias = true;
  #   configure = (import ./vim/customization.nix { pkgs = super; });
  # };

  plex = super.plex.overrideAttrs (x: rec {
    name = "plex-${version}";
    version = "1.13.2.5154";
    vsnHash = "fd05be322";

    src = super.fetchurl {
      url = "https://downloads.plex.tv/plex-media-server/${version}-${vsnHash}/plexmediaserver-${version}-${vsnHash}.x86_64.rpm";
      sha256 = "09hy9xhhv17jbzplyls13xrzaxndlc278amp6k3w8q4j6wpsi6np";
    };
  });

  # This causes a lot of rebuilds
  # gnupg = super.gnupg.override (x: {
  #   pinentry = self.pinentry_qt5;
  # });

  powermate = callPackage ../pkgs/powermate-linux { };

  sddm-theme-breeze-custom = callPackage ../pkgs/sddm-theme-breeze-custom { };

  slim-themes = callPackage ../pkgs/slim-themes { };

  steamcontroller-udev-rules = callPackage ../pkgs/steamcontroller-udev-rules { };

  # texlive = super.texlive.combine {
  #   inherit (super.texlive) scheme-small wrapfig capt-of cm-super;
  # };

  thunderbolt = callPackage ../pkgs/thunderbolt { };

  # yaml2json = callPackage ../pkgs/yaml2json { };

}
