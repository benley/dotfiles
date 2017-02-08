# https://nixos.org/wiki/QEMU_guest_with_VGA_passthrough
{ config, pkgs, ... }:

{
  boot.kernelModules = [
    "vfio"
    "vfio_pci"
    "vfio_iommu_type1"
  ];

  boot.blacklistedKernelModules = [
    "nouveau"
    "nvidia"
  ];

  boot.kernelParams = [
    "intel_iommu=on"
    "vfio_iommu_type1.allow_unsafe_interrupts=1"
    "kvm.allow_unsafe_assigned_interrupts=1"
    "kvm.ignore_msrs=1" # This prevents certain BSOD crashes in Windows guests.
    "i915.enable_hd_vgaarb=1"  # Is this still required?
  ];
}
