{ config, pkgs, lib, ... }:

# Stuff for "workstation" machines (not necessarily GUI though; these can be headless)
{
  imports = [
    ./yubikey.nix
  ];

  environment.systemPackages = with pkgs; [
    gdb
    git-lfs
    gitFull
    gnumake
    p7zip
    python3
    # python3Packages.autopep8
    # python3Packages.flake8
    # python3Packages.pep8
    python3Packages.pygments  # for LaTeX minted/pygmentex
    # python3Packages.pylint
    # python3Packages.virtualenv
    # python3Packages.virtualenvwrapper
    texlive-for-orgmode
  ];
}
