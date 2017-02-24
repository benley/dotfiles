{ config, pkgs, ... }:

{
  users.extraUsers.benley = {
    isNormalUser = true;
    uid = 1000;
    description = "Benjamin Staffin";
    extraGroups = [ "wheel" "vboxusers" ];
    initialHashedPassword =
      ''$6$cfJfcXyI7KvBD$1MuMJk3VBn.VeF6LNbeuLeo0BZGzdZbz4AP/UDChAnlI73onkiIxDUthkAFUtR/RN.20.lnyS3550EEPbVIVQ.'';
  };
}
