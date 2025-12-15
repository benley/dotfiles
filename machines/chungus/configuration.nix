# Chungus: Thinkpad P16s gen 2 (AMD)

{ config, pkgs, lib, inputs, ... }:

{
  imports = [
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-p16s-amd-gen2
    inputs.home-manager.nixosModules.default
    ./hardware-configuration.nix
    ../imports/defaults.nix
    ../imports/graphical.nix
    # ../imports/redshift.nix
  ];

  hardware.enableAllFirmware = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.configurationLimit = 10;

  networking.hostName = "chungus";
  networking.domain = "ws.memcompute.com";
  networking.hostId = "007f0200";

  # I will probably need this (for netgroups) if I want sudo rules to work
  services.nscd.enableNsncd = false;

  # Allow changing timezone via dbus, so GeoIP location detection can work
  time.timeZone = null;

  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  networking.useDHCP = false;
  networking.networkmanager.enable = true;

  services.resolved.enable = true;
  services.resolved.llmnr = "false";
  services.resolved.extraConfig = ''
    MulticastDNS=true
  '';

  services.avahi.nssmdns4 = false;
  services.avahi.publish.addresses = false;
  services.avahi.publish.enable = false;

  services.libinput.enable = true;
  services.libinput.touchpad.naturalScrolling = true;

  services.xserver = {
    enable = true;
    xkb.options = "ctrl:nocaps";
  };

  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  virtualisation.docker.enable = true;

  # evdev:atkbd:... modalias string comes from `evemu-describe /dev/input/event0`
  # (it comes from DMI data, you can probably also find it with `cat /sys/class/dmi/id/modalias`)
  services.udev.extraHwdb = ''
    # Remap PrtSc (which is bizarrely between right-alt and right-ctl) to right-meta
    # evdev:atkbd:dmi:bvn*:bvr*:bd*:svn*:pn*:pvrThinkPadP16sGen2*
    #   KEYBOARD_KEY_b7=rightmeta

    # Remap fn-f11 and fn-12 to something usable (XF86Launch1, XF86Launch2)
    evdev:name:ThinkPad Extra Buttons:dmi:bvn*:bvr*:bd*:svnLENOVO*:pn*
      KEYBOARD_KEY_49=prog1
      KEYBOARD_KEY_45=prog2
  '';

  services.fwupd.enable = true;

  system.stateVersion = "24.11"; # Don't change this post-install

  users.users.bstaffin = {
    isNormalUser = true;
    uid = 185476231;
    group = "bstaffin";
    description = "Benjamin Staffin";
    extraGroups = [ "docker" "wheel" "systemd-journal" "networkmanager" "libvirtd" ];

    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCcswZFgoqkYR6RUpsQCH8hczzzCIQHzDPTzR3G2PcvtMBeCrArSGI1/ZKaqmxNBSwNvQiBM1ze3xGDV0tye7qRDJZPMzJE6dYw4fcaz7yBXfj67jmCPhPtgKkRNcxmWbyw48TyeQG4pqLZlAgaPQBPr8zhGX/NMlDM7tqKuGwRswAOj2kUdJIoAKus+iYvgUjgpKBsZWCNhq+gydPIJq78cz1H3HPbmtIoVyiN8b/jihbsY7v/4oy75zOWPG6WHGcZPNnzVKr8LFdAWva86K6ZL2//gkSRvceOFwFmAUnRF7zdtu+55+gjXkcLDzrNPUDhnGOKRfCaf9gZL38IbHrxG67tYRC9LdSlj0kwgBWo8ChtTVe2f8VrUoy/phKs9YSweYEjXzgVmp+OIgdy4204crDKmD+pUfDi8G8QAP4IUj/IiKML+UJmLfqKlOxsLgYMKLEwynaLTv5PuOLC0Y2k5/icdB5ubk7wL+XcLMYI0c9NzZOTwkiPL8v+QTYil81pXj1HY9gxv2AYGiD2aW0pds72BcI101JG+t45UXAksSXsET51eyMyQYVH+hkO15X/FFL0AZMRJxPgW0uVI1S2njvJ3VuKeoww4plGSzDz+hxi94MShPYP6oa5zokgl+pJjwia5kTzh9nM2Ks8REuI+ugzWPOaK+HdMCRenBLq9w== openpgp:0x7FB34A50"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC/eeM61Woa0ilIFJ64DibnjKObxMBGz2sY0YvlxgepymsPaIqKpZm1YUhV8YAN1R3GWzmQBts+z/Z/IJ4y/fGzd9IjuVHb25g6ECsqratjYvvvLAQZe7YwXW8XICK8/5FpPruHGJUvvelkjSBVrKOleuLuu0DJnLN9v+zcU8lOSCx4ucMeIDuBK6IbRNgxk4iuGmGTjXVxg5uQGUC9QutWzqrC9+zXQnm0LYCOyUIxyIq4IQR+dtsM40nBUv6hWj+60xYmmrr0q3gWVfWEDux0hLAGDkQWYqvfkQBZ0Fh43ULlhKp9RLiNj2r8Cppkm4SPLjXx+vCIX7zWEgSHRVxt/B/StRWaHWFqB9UXE0BrmEzQ9OA6DJ++5Ok24Zh1k3BFRTJ0M/+a7if/RnB2EFvO+Zg0j3zJmXKfhOkCQlWEWk0j5wQ0D6bzUVEmbPLnxMzsMHgzJtf5FcKk+QytPQXtWzhs3+KjP1zuOGzB60YuMI9vSZfbF5ti/a97a0sZPyfuKGu4HtFfhs/yWR8BKjxbT6gXxEEmqsXxvJ/il/FwqV9jc8tC90WqK/aOJRJHujE5hfISkAGs77NcoTTE9shZfdb99CaUzRO/YdkCzP/CFf8fdXd/bc+G8lAWdNnbN1KADRr+v2h1CAV4Is2VUemJSKAhWvZ9SFJhT8NycEfVWw== bstaffin@singlestore.com"
    ];
  };

  users.groups.bstaffin.gid = 185476231;

  home-manager.useUserPackages = true;
  home-manager.users.bstaffin = import ../../home.nix;

  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 14d";

  nix.settings.trusted-users = [ "bstaffin" "benley" ];

  environment.systemPackages = with pkgs; [
    tofu-ls
    # terraform-ls
    virt-manager
    # inputs.memsql-provisioning.packages."${pkgs.system}".memsqlaws
    virtiofsd
  ];

  # programs.obs-studio = {
  #   enable = true;
  #   enableVirtualCamera = true;
  # };

  # build broken 2025-07-18
  # services.ddccontrol.enable = true;

  services.openssh.enable = true;

  services.tailscale.enable = true;

  virtualisation.libvirtd.enable = true;

  security.ipa = {
    enable = true;
    server = "ops-ova-ns-2.ops.memcompute.com";
    realm = "OPS.MEMCOMPUTE.COM";
    domain = "ops.memcompute.com";
    certificate = pkgs.fetchurl {
      url = "https://idm.cloud.memcompute.com/ipa/config/ca.crt";
      sha256 = "0ngzrxjwsrjfp2bh4s324yij16sbrphh8c1sqgcq3nmxq1j60am6";
    };
    basedn = "dc=ops,dc=memcompute,dc=com";
    dyndns.interface = "tailscale0";
  };

  services.sssd.sshAuthorizedKeysIntegration = true;

  # I think systemd-user covers all the ways someone might login
  security.pam.services.systemd-user.makeHomeDir = true;
  security.sudo.package = pkgs.sudo.override { withSssd = true; };
  system.nssDatabases.sudoers = [ "sss" ];

  # see nixpkgs#309612
  environment.etc."nsswitch.conf".text = ''
    netgroup: files sss
  '';

}
