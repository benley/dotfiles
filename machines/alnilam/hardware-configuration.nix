# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usbhid" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/6f52d6b6-620d-472f-92f9-8492d8495324";
      fsType = "ext4";
    };

  boot.initrd.luks.devices."crypt_dev_nvme0n1p4".device = "/dev/disk/by-uuid/78c6cfde-fe44-4c6f-81fc-f4d7e2556cc8";

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/a5bcd95f-277c-4486-ab42-ed7b6f7196ea";
      fsType = "ext2";
    };

  fileSystems."/boot/efi" =
    { device = "/dev/disk/by-uuid/27C4-727C";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/059d94b7-10c2-4bab-ab6d-f10506f8d377"; }
    ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}