{ config, pkgs, ... }:

{
  services.redshift = {
    enable = true;
    latitude = "42.3601";
    longitude = "-71.0589";
    temperature.day = 6500;
    temperature.night = 3700;
  };
}
