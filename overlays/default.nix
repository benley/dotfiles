self: super:

with { callPackage = super.callPackage; };

{
  awsudo = super.pythonPackages.callPackage ../pkgs/awsudo { };

  eless = super.writeScriptBin "eless" (builtins.readFile ../eless.sh);

  kubernetes-client = callPackage ../pkgs/kubernetes-client { };

  kupfer-plugin-google-search = callPackage ../pkgs/kupfer-plugin-google-search { };

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
    version = "1.13.4.5251";
    vsnHash = "2e6e8f841";

    src = super.fetchurl {
      url = "https://downloads.plex.tv/plex-media-server/${version}-${vsnHash}/plexmediaserver-${version}-${vsnHash}.x86_64.rpm";
      sha256 = "4be3f55b246b0af81fdcdcf49329926ed2d54e374b0544d5c090718af85e161a";
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

  git-credential-libsecret = super.git.overrideAttrs (x: rec {
    name = "git-credential-libsecret-${self.lib.getVersion super.git.name}";
    buildInputs = with self; [ pkgconfig glib libsecret ];
    buildPhase = ''
      make -C contrib/credential/libsecret
    '';
    installPhase = ''
      mkdir -p $out/bin
      mv contrib/credential/libsecret/git-credential-libsecret $out/bin/
    '';
  });
}
