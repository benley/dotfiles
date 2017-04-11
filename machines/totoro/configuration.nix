{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../imports/defaults.nix
    ../imports/graphical.nix
    ../imports/kde.nix
    ../imports/wacom.nix
    ../imports/trackpoint.nix
    ../imports/virtualbox.nix
  ];

  boot.loader.efi = {
    canTouchEfiVariables = true;
    efiSysMountPoint = "/boot/efi";
  };

  boot.loader.grub = {
    enable = true;
    enableCryptodisk = true;
    devices = [ "/dev/sda" ];
    efiSupport = true;
  };

  boot.supportedFilesystems = [ "zfs" ];

  boot.initrd.luks = {
    mitigateDMAAttacks = true;
    devices = [{
      name = "crypted";
      device = "/dev/disk/by-uuid/9a2723c0-9b4e-47cc-897a-70edd09904dd";
      allowDiscards = true;
    }];
  };

  networking.hostId = "8425e349";
  networking.hostName = "totoro";

  i18n.consoleFont = "Lat2-Terminus16";

  services.xserver = {
    useGlamor = true;
    libinput.enable = true;
    videoDrivers = [ "modesetting" ];

    dpi = 120;  # Actual dpi is something like 157 but that looks way too huge

    xkbOptions = "ctrl:nocaps,compose:ralt";
  };

  hardware.opengl = {
    extraPackages = with pkgs;
        [ vaapiIntel libvdpau libvdpau-va-gl vaapiVdpau ];
    extraPackages32 = with pkgs.pkgsi686Linux;
        [ vaapiIntel libvdpau libvdpau-va-gl vaapiVdpau ];

    s3tcSupport = true;
  };

  environment.variables = {
    # There is no dedicated i965 vpdau driver, so this tells libvpdau to use
    # libva as its backend.
    VDPAU_DRIVER = "va_gl";
  };

  hardware.bluetooth.enable = true;

  # I'm not sure if these next two are necessary:
  hardware.enableAllFirmware = true;
  hardware.cpu.intel.updateMicrocode = true;

  system.stateVersion = "17.03";

  zramSwap.enable = true;
}
