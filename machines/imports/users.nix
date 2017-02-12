{ config, pkgs, ... }:

{
  users.extraUsers.benley = {
    isNormalUser = true;
    uid = 1000;
    description = "Benjamin Staffin";
    extraGroups = [ "wheel" "vboxusers" ];
  };
}
