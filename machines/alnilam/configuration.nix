# Alnilam: Thinkpad T490 (intel graphics)

{ config, pkgs, lib, ... }:

{
  imports = [
    <nixos-hardware/lenovo/thinkpad/t490>
    <home-manager/nixos>
    ./hardware-configuration.nix
    ../imports/defaults.nix
    ../imports/graphical.nix
    ../imports/redshift.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  networking.hostName = "alnilam";
  networking.hostId = "6a9e86d2";

  # Allow changing timezone via dbus, so GeoIP location detection can work
  time.timeZone = null;

  networking.useDHCP = false;
  networking.networkmanager.enable = true;

  services.resolved.enable = true;
  services.resolved.extraConfig = ''
    MulticastDNS=true
  '';

  services.avahi.nssmdns = false;

  services.xserver = {
    enable = true;
    libinput.enable = true;
    libinput.touchpad.naturalScrolling = true;

    videoDrivers = [ "intel" ];  # TODO: why intel instead of modesetting?

    dpi = 144;  # gnome3 seems to completely ignore this

    xkbOptions = lib.concatStringsSep "," [
      "ctrl:nocaps"
    ];
  };

  hardware.opengl = {
    extraPackages = with pkgs;
        [ vaapiIntel libvdpau libvdpau-va-gl vaapiVdpau ];
    extraPackages32 = with pkgs.pkgsi686Linux;
        [ vaapiIntel libvdpau libvdpau-va-gl vaapiVdpau ];
  };

  environment.variables = {
    # There is no dedicated i965 vpdau driver, so this tells libvpdau to use
    # libva as its backend.
    VDPAU_DRIVER = "va_gl";
  };

  hardware.bluetooth.enable = true;
  hardware.cpu.intel.updateMicrocode = true;

  virtualisation.docker.enable = true;

  # evdev:atkbd:... modalias string comes from `evemu-describe /dev/input/event0`
  # (it comes from DMI data, you can probably also find it with `cat /sys/class/dmi/id/modalias`)
  services.udev.extraHwdb = ''
    # Remap PrtSc (which is bizarrely between right-alt and right-ctl) to right-meta
    evdev:atkbd:dmi:bvn*:bvr*:bd*:svn*:pn*:pvrThinkPadT490*
      KEYBOARD_KEY_b7=rightmeta

    # Remap fn-f11 and fn-12 to something usable (XF86Launch1, XF86Launch2)
    evdev:name:ThinkPad Extra Buttons:dmi:bvn*:bvr*:bd*:svnLENOVO*:pn*
      KEYBOARD_KEY_49=prog1
      KEYBOARD_KEY_45=prog2
  '';

  services.powermate.enable = true;

  services.fwupd.enable = true;

  system.stateVersion = "20.09"; # Don't change this post-install

  users.users.bstaffin = {
    isNormalUser = true;
    uid = 185476231;
    group = "bstaffin";
    description = "Benjamin Staffin";
    extraGroups = [ "docker" "wheel" "vboxusers" "systemd-journal" "networkmanager" ];

    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCcswZFgoqkYR6RUpsQCH8hczzzCIQHzDPTzR3G2PcvtMBeCrArSGI1/ZKaqmxNBSwNvQiBM1ze3xGDV0tye7qRDJZPMzJE6dYw4fcaz7yBXfj67jmCPhPtgKkRNcxmWbyw48TyeQG4pqLZlAgaPQBPr8zhGX/NMlDM7tqKuGwRswAOj2kUdJIoAKus+iYvgUjgpKBsZWCNhq+gydPIJq78cz1H3HPbmtIoVyiN8b/jihbsY7v/4oy75zOWPG6WHGcZPNnzVKr8LFdAWva86K6ZL2//gkSRvceOFwFmAUnRF7zdtu+55+gjXkcLDzrNPUDhnGOKRfCaf9gZL38IbHrxG67tYRC9LdSlj0kwgBWo8ChtTVe2f8VrUoy/phKs9YSweYEjXzgVmp+OIgdy4204crDKmD+pUfDi8G8QAP4IUj/IiKML+UJmLfqKlOxsLgYMKLEwynaLTv5PuOLC0Y2k5/icdB5ubk7wL+XcLMYI0c9NzZOTwkiPL8v+QTYil81pXj1HY9gxv2AYGiD2aW0pds72BcI101JG+t45UXAksSXsET51eyMyQYVH+hkO15X/FFL0AZMRJxPgW0uVI1S2njvJ3VuKeoww4plGSzDz+hxi94MShPYP6oa5zokgl+pJjwia5kTzh9nM2Ks8REuI+ugzWPOaK+HdMCRenBLq9w== openpgp:0x7FB34A50"
    ];
  };

  users.groups.bstaffin.gid = 185476231;

  home-manager.useUserPackages = true;
  home-manager.users.bstaffin = import ../../home.nix;

  # programs.sway.enable = true;

  # https://github.com/NixOS/nixpkgs/issues/111835
  # https://github.com/rancher/k3d/issues/493
  systemd.enableUnifiedCgroupHierarchy = false;

  services.fprintd.enable = true;
}
