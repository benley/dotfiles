{ config, pkgs, ... }:

{
  services.redshift = {
    enable = true;
    latitude = "37.77493";
    longitude = "-122.41942";
    temperature.day = 6500;
    temperature.night = 3500;
  };
}
