{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../imports/defaults.nix
    ../imports/graphical.nix
    ../imports/kde.nix
    ../imports/wacom.nix
    ../imports/trackpoint.nix
  ];

  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

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
      device = "/dev/sda3";
      allowDiscards = true;
    }];
  };

  networking.hostId = "8425e349";

  networking.hostName = "totoro";
  networking.networkmanager.enable = true;

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [
  ];

  services.avahi = {
    enable = true;
    ipv4 = true;
    ipv6 = true;
    nssmdns = true;
  };

  services.xserver = {
    enable = true;
    useGlamor = true;
    layout = "us";
    libinput.enable = true;
    videoDrivers = [ "modesetting" ];
  };

  hardware.opengl = {
    extraPackages = with pkgs;
        [ vaapiIntel libvdpau libvdpau-va-gl vaapiVdpau ];
    extraPackages32 = with pkgs.pkgsi686Linux;
        [ vaapiIntel libvdpau libvdpau-va-gl vaapiVdpau ];

    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    s3tcSupport = true;
  };

  environment.variables = {
    # There is no dedicated i965 vpdau driver, so this tells libvpdau to use
    # libva as its backend.
    VDPAU_DRIVER = "va_gl";
  };

  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;
  hardware.bluetooth.enable = true;

  # I'm not sure if these next two are necessary:
  hardware.enableAllFirmware = true;
  hardware.cpu.intel.updateMicrocode = true;

  system.stateVersion = "17.03";

  zramSwap.enable = true;
}
