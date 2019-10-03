{ config, pkgs, lib, ... }:

{
  location.provider = "geoclue2";

  services.redshift = {
    enable = true;
    temperature.day = 6500;
    temperature.night = 3200;
  };
}
