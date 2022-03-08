{ config, pkgs, ... }:

{
  imports = [
    <home-manager/nixos>
  ];

  users.users.benley = {
    isNormalUser = true;
    uid = 1000;
    description = "Benjamin Staffin";
    extraGroups = [ "docker" "wheel" "vboxusers" "systemd-journal" "networkmanager" "libvirtd" ];

    # Just a bootstrapping password, not one I actually use for anything:
    initialHashedPassword =
      ''$6$cfJfcXyI7KvBD$1MuMJk3VBn.VeF6LNbeuLeo0BZGzdZbz4AP/UDChAnlI73onkiIxDUthkAFUtR/RN.20.lnyS3550EEPbVIVQ.'';

    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCcswZFgoqkYR6RUpsQCH8hczzzCIQHzDPTzR3G2PcvtMBeCrArSGI1/ZKaqmxNBSwNvQiBM1ze3xGDV0tye7qRDJZPMzJE6dYw4fcaz7yBXfj67jmCPhPtgKkRNcxmWbyw48TyeQG4pqLZlAgaPQBPr8zhGX/NMlDM7tqKuGwRswAOj2kUdJIoAKus+iYvgUjgpKBsZWCNhq+gydPIJq78cz1H3HPbmtIoVyiN8b/jihbsY7v/4oy75zOWPG6WHGcZPNnzVKr8LFdAWva86K6ZL2//gkSRvceOFwFmAUnRF7zdtu+55+gjXkcLDzrNPUDhnGOKRfCaf9gZL38IbHrxG67tYRC9LdSlj0kwgBWo8ChtTVe2f8VrUoy/phKs9YSweYEjXzgVmp+OIgdy4204crDKmD+pUfDi8G8QAP4IUj/IiKML+UJmLfqKlOxsLgYMKLEwynaLTv5PuOLC0Y2k5/icdB5ubk7wL+XcLMYI0c9NzZOTwkiPL8v+QTYil81pXj1HY9gxv2AYGiD2aW0pds72BcI101JG+t45UXAksSXsET51eyMyQYVH+hkO15X/FFL0AZMRJxPgW0uVI1S2njvJ3VuKeoww4plGSzDz+hxi94MShPYP6oa5zokgl+pJjwia5kTzh9nM2Ks8REuI+ugzWPOaK+HdMCRenBLq9w== openpgp:0x7FB34A50"
    ];
  };

  users.groups.warez = {
    gid = 1002;
    members = [ "benley" "transmission" ];
  };

  home-manager.useUserPackages = true;
  home-manager.users.benley = import ../../home.nix;
  # home-manager.users.benley = { pkgs, ... }: {
    # services.emacs.enable = true;

    # xresources.extraConfig = builtins.readFile ../../cfg/.Xresources;

    # dconf.settings = {

    #   "org/gnome/desktop/interface" = {
    #     "scaling-factor" = 1;
    #     "text-scaling-factor" = 1.5;
    #   };

    #   "org/gnome/desktop/wm/keybindings" = {
    #     close = [ "<Super>W" "<Alt>F4" ];
    #     cycle-group = [ "<Alt>Above_Tab" ];
    #     cycle-group-backward = [ "<Shift><Alt>Above_Tab" ];
    #     show-desktop = [ "<Super><Shift>H" ];
    #     # Ideally these two would be empty lists but home-manager doesn't know
    #     # how to add GVariant type annotations. In the dconf.ini file it would
    #     # need to come out as:
    #     #    switch-group=@as []
    #     #    switch-group-backward=@as []
    #     switch-group = [""];
    #     switch-group-backward = [""];
    #     toggle-fullscreen = [ "F11" ];
    #   };

    #   "org/gnome/desktop/wm/preferences" = {
    #     "button-layout" = "close,minimize,maximize:appmenu";
    #     "focus-mode" = "click";
    #     "resize-with-right-button" = true;
    #     "titlebar-font" = "Cantarell Italic 11";  # does this actually do anything?
    #   };

    #   "org/gnome/desktop/datetime" = {
    #     automatic-timezone = true;
    #   };

    #   "org/gnome/settings-daemon/plugins/media-keys" = {

    #     "custom-keybindings" = [
    #       "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/"
    #       "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/"
    #       "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/"
    #     ];

    #     "logout" = "<Shift><Super>Q";
    #     "screenshot" = "<Shift><Super>numbersign";
    #     "area-screenshot" = "<Shift><Super>dollar";
    #     "window-screenshot" = "<Shift><Super>percent";
    #   };

    #   "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
    #     binding = "<Shift><Super>Return";
    #     command = "gnome-terminal";
    #     name = "Open Terminal";
    #   };

    #   "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1" = {
    #     binding = "<Shift><Super>E";
    #     command = "emacsclient -n -c";
    #     name = "New emacsclient frame";
    #   };

    #   "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2" = {
    #     binding = "<Shift><Super>N";
    #     command = "networkmanager_dmenu";
    #     name = "networkmanager_dmenu";
    #   };
    # };
  # };
}
