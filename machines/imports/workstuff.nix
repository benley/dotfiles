{ config, pkgs, ... }:

{
  networking.networkmanager.dns = "dnsmasq";
  networking.networkmanager.enableStrongSwan = true;

  environment.etc = [{
    target = "NetworkManager/dnsmasq.d/work-vpn.conf";
    text = builtins.readFile ../../private/work-vpn.dnsmasq.conf;
  }];

  # services.unbound.enable = true;
  # services.unbound.extraConfig = builtins.readFile ../../private/postmates.unbound.conf;
}
