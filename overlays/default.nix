self: super:

with { callPackage = super.callPackage; };

{
  adafruit-udev-rules = callPackage ../pkgs/adafruit-udev-rules { };

  awsudo = super.pythonPackages.callPackage ../pkgs/awsudo { };

  battery-monitor = super.pythonPackages.callPackage ../pkgs/battery-monitor { };

  # dunst = super.dunst.override {
  #   dunstify = true;
  # };

  eless = super.writeScriptBin "eless" (builtins.readFile ../eless.sh);

  # insync = callPackage ../pkgs/insync { };

  # insync-v3 = self.libsForQt5.callPackage ../pkgs/insync/insync-v3.nix { };

  kubernetes-client = callPackage ../pkgs/kubernetes-client { };

  kupfer-plugin-google-search = callPackage ../pkgs/kupfer-plugin-google-search { };

  mosquitto-exporter = callPackage ../pkgs/mosquitto-exporter { };

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

  haskellPackages = let pkgs = super; in super.haskellPackages.override {
    overrides = self: super: {
      taffybar-plugins = self.callPackage ../pkgs/taffybar-plugins {};
    };
  };

  taffybar = super.taffybar.override (_: {
    packages = _: [self.haskellPackages.taffybar-plugins];
  });

  terminfo-extras = callPackage ../pkgs/terminfo-extras { } ;
}
