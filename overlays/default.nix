self: super:

with { callPackage = super.callPackage; };

{
  adafruit-udev-rules = callPackage ../pkgs/adafruit-udev-rules { };

  awsudo = super.pythonPackages.callPackage ../pkgs/awsudo { };

  battery-monitor = super.pythonPackages.callPackage ../pkgs/battery-monitor { };


  kupfer-plugin-google-search = callPackage ../pkgs/kupfer-plugin-google-search { };
  ddccontrol-db = super.ddccontrol-db.overrideAttrs (x: rec {
    postInstall = ''
      cp ${../pkgs/ddccontrol-db/ACI32A4.xml} $out/share/ddccontrol-db/monitor/ACI32A4.xml
    '';
  });

  mosquitto-exporter = callPackage ../pkgs/mosquitto-exporter { };

  powermate = callPackage ../pkgs/powermate-linux { };

  sddm-theme-breeze-custom = callPackage ../pkgs/sddm-theme-breeze-custom { };

  slim-themes = callPackage ../pkgs/slim-themes { };

  steamcontroller-udev-rules = callPackage ../pkgs/steamcontroller-udev-rules { };

  # texlive = super.texlive.combine {
  #   inherit (super.texlive) scheme-small wrapfig capt-of cm-super;
  # };

  # yaml2json = callPackage ../pkgs/yaml2json { };

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
