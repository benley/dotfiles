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
    # dropbox-cli
    # exercism
    gdb
    gitFull
    go
    google-cloud-sdk
    gotags
    # haskellPackages.ghc
    # haskellPackages.ghc-mod  # broken?
    # haskellPackages.hdevtools # broken?
    # haskellPackages.hindent
    # haskellPackages.hlint
    haskellPackages.ShellCheck
    html-tidy
    jsonnet
    gnumake
    nodePackages.js-yaml
    nodePackages.jshint
    pandoc
    # python27Full
    python3Full
    python3Packages.autopep8
    python3Packages.flake8
    python3Packages.pep8
    python3Packages.pylint
    python3Packages.python-language-server
    python3Packages.virtualenv
    python3Packages.virtualenvwrapper
    stack
    imagemagick  # so emacs can resize images
    lastpass-cli
  ];

  services.emacs.package = pkgs.fancyEmacs;
}
