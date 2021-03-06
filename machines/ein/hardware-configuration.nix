# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot.initrd.availableKernelModules = [ "ahci" "usbhid" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  boot.zfs.extraPools = [ "pool0" ];

  fileSystems."/" =
    { device = "rpool/root/nixos";
      fsType = "zfs";
    };

  fileSystems."/var/lib/docker" =
    { device = "pool0/docker";
      fsType = "zfs";
    };

  fileSystems."/home" =
    { device = "pool0/home";
      fsType = "zfs";
    };

  fileSystems."/home/benley" =
    { device = "rpool/home/benley";
      fsType = "zfs";
    };

  fileSystems."/home/nix" =
    { device = "pool0/home/nix";
      fsType = "zfs";
    };

  fileSystems."/pool0" =
    { device = "pool0";
      fsType = "zfs";
    };

  fileSystems."/pool0/vm" =
    { device = "pool0/vm";
      fsType = "zfs";
    };

  fileSystems."/pool0/steamlibrary" =
    { device = "pool0/steamlibrary";
      fsType = "zfs";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/7979ecc8-8020-4d79-8ec9-3d4d141bc3b5";
      fsType = "ext4";
    };

  fileSystems."/boot/efi" =
    { device = "/dev/disk/by-uuid/E69A-38AE";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/017d2803-feac-4a80-bc1e-85b015e52ff0"; }
    ];

  nix.maxJobs = lib.mkDefault 8;
}
