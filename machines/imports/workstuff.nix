{ config, pkgs, ... }:

{
  networking.networkmanager.useDnsmasq = true;

  environment.etc = [{
    target = "NetworkManager/dnsmasq.d/work-vpn.conf";
    text = builtins.readFile ../../private/work-vpn.dnsmasq.conf;
  }];
}