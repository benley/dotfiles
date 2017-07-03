{ config, pkgs, ... }:

{
  users.users.benley = {
    isNormalUser = true;
    uid = 1000;
    description = "Benjamin Staffin";
    extraGroups = [ "docker" "wheel" "vboxusers" "systemd-journal" ];

    # Just a bootstrapping password, not one I actually use for anything:
    initialHashedPassword =
      ''$6$cfJfcXyI7KvBD$1MuMJk3VBn.VeF6LNbeuLeo0BZGzdZbz4AP/UDChAnlI73onkiIxDUthkAFUtR/RN.20.lnyS3550EEPbVIVQ.'';

    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCcswZFgoqkYR6RUpsQCH8hczzzCIQHzDPTzR3G2PcvtMBeCrArSGI1/ZKaqmxNBSwNvQiBM1ze3xGDV0tye7qRDJZPMzJE6dYw4fcaz7yBXfj67jmCPhPtgKkRNcxmWbyw48TyeQG4pqLZlAgaPQBPr8zhGX/NMlDM7tqKuGwRswAOj2kUdJIoAKus+iYvgUjgpKBsZWCNhq+gydPIJq78cz1H3HPbmtIoVyiN8b/jihbsY7v/4oy75zOWPG6WHGcZPNnzVKr8LFdAWva86K6ZL2//gkSRvceOFwFmAUnRF7zdtu+55+gjXkcLDzrNPUDhnGOKRfCaf9gZL38IbHrxG67tYRC9LdSlj0kwgBWo8ChtTVe2f8VrUoy/phKs9YSweYEjXzgVmp+OIgdy4204crDKmD+pUfDi8G8QAP4IUj/IiKML+UJmLfqKlOxsLgYMKLEwynaLTv5PuOLC0Y2k5/icdB5ubk7wL+XcLMYI0c9NzZOTwkiPL8v+QTYil81pXj1HY9gxv2AYGiD2aW0pds72BcI101JG+t45UXAksSXsET51eyMyQYVH+hkO15X/FFL0AZMRJxPgW0uVI1S2njvJ3VuKeoww4plGSzDz+hxi94MShPYP6oa5zokgl+pJjwia5kTzh9nM2Ks8REuI+ugzWPOaK+HdMCRenBLq9w== openpgp:0x7FB34A50"
    ];
  };
}
