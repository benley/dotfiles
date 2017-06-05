#### ALNITAK - Razer Blade Stealth (late 2016) ####

{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../imports/defaults.nix
    ../imports/graphical.nix
    ../imports/kde.nix
    ../imports/wacom.nix
    ../imports/workstuff.nix
    ../imports/virtualbox.nix
  ];

  boot.kernelPackages = pkgs.linuxPackages_4_11;
  # zfs 0.6.5.9 doesn't work with linux 4.11 but 0.7.0rc4 does
  boot.zfs.enableUnstable = true;

  # Make the cryptsetup password prompt readable
  boot.earlyVconsoleSetup = true;

  boot.loader.efi = {
    canTouchEfiVariables = true;
    efiSysMountPoint = "/boot/efi";
  };

  boot.loader.grub = {
    enable = true;
    enableCryptodisk = true;
    devices = [ "/dev/nvme0n1" ];
    efiSupport = true;
    zfsSupport = true;
    version = 2;
    gfxmodeEfi = "3840x2160";

    #font = "${pkgs.font-droid}/share/fonts/droid/DroidSansMono.ttf";
    font = ./PragmataPro_Mono_R_0821.ttf;
    fontSize = 48;
    splashImage = null;

    useOSProber = true;
  };

  boot.supportedFilesystems = [ "zfs" ];

  boot.initrd.luks = {
    mitigateDMAAttacks = true;
    devices = [{
      name = "crypted";
      device = "/dev/nvme0n1p7";
      allowDiscards = true;
    }];
  };

  networking.hostId = "8425e349";
  networking.hostName = "alnitak";

  i18n.consoleFont = "ter-132b";
  i18n.consolePackages = [ pkgs.terminus_font ];

  services.printing.enable = true;

  services.xserver = {
    useGlamor = true;
    libinput.enable = true;
    videoDrivers = [ "modesetting" ];

    dpi = 240;  # Physical dpi is ~352 but as usual that makes the UI too big

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
  # hardware.enableAllFirmware = true;
  # hardware.cpu.intel.updateMicrocode = true;

  system.stateVersion = "17.03";

  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";

  zramSwap.enable = true;
}
