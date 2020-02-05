{ config, pkgs, lib, ... }:

# Stuff for "workstation" machines (not necessarily GUI though; these can be headless)
{
  imports = [
    ./yubikey.nix
  ];

  environment.systemPackages = with pkgs; [
    cabal-install
    cabal2nix
    ctags
    dropbox-cli
    exercism
    gdb
    gitFull
    go
    google-cloud-sdk
    gotags
    haskellPackages.ghc
    # haskellPackages.ghc-mod  # broken?
    # haskellPackages.hdevtools # broken?
    haskellPackages.hindent
    haskellPackages.hlint
    haskellPackages.ShellCheck
    html-tidy
    jsonnet
    gnumake
    nodePackages.js-yaml
    nodePackages.jshint
    pandoc
    python27Full
    python3Full
    pythonPackages.autopep8
    pythonPackages.flake8
    pythonPackages.pep8
    python3Packages.pylint
    pythonPackages.virtualenv
    pythonPackages.virtualenvwrapper
    stack
    imagemagick  # so emacs can resize images
    lastpass-cli
  ];

  services.emacs.package = pkgs.fancyEmacs;
}
