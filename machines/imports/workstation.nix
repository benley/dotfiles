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
    git-lfs
    go
    google-cloud-sdk
    google-drive-ocamlfuse
    # haskellPackages.ghc
    # haskellPackages.ghc-mod  # broken?
    # haskellPackages.hdevtools # broken?
    # haskellPackages.hindent
    # haskellPackages.hlint
    html-tidy
    gnumake
    nodePackages.js-yaml
    nodePackages.jshint
    p7zip
    pandoc
    # python27Full
    python3Full
    python3Packages.autopep8
    python3Packages.flake8
    python3Packages.pep8
    python3Packages.pylint
    # python3Packages.python-language-server
    python3Packages.virtualenv
    python3Packages.virtualenvwrapper
    stack
    imagemagick  # so emacs can resize images
    python3Packages.pygments  # for LaTeX minted/pygmentex
    lastpass-cli
    texlive-for-orgmode
  ];

  #services.emacs.package = pkgs.fancyEmacs;
}
