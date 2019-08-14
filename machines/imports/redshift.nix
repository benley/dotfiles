{ config, pkgs, lib, ... }:

{
  services.redshift = {
    enable = true;
    latitude = lib.mkDefault "42.3601";
    longitude = lib.mkDefault "-71.0589";
    temperature.day = 6500;
    temperature.night = 3700;
  };
}
