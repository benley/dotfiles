{
  allowUnfree = true;

  #allowBroken = true;

  firefox.enableGoogleTalkPlugin = true;
  firefox.enableGnomeExtensions = true;
  #firefox.enableAdobeFlash = true;

  chromium.enablePepperFlash = true;
  chromium.enablePepperPDF = true;
  chromium.enableWideVine = true;

  packageOverrides = origPkgs: with origPkgs; {
    benleyAll = with pkgs; buildEnv {
      name = "benleyAll";
      paths = [
        bashInteractive
        bashCompletion
        bc
        #benleyDesktop
        benleyDevTools
        #benleyGuiStuff
        benleyHaskellDev
        benleyNetTools
        #benleyPythonDev
        #benleySystemTools
        jq
        myScripts
        pwgen
        tmux
        unzip
      ];
    };

    benleyNetTools = with pkgs; buildEnv {
      name = "benleyNetTools";
      paths = [
        awscli
        #cli53
        curl
        httpie
        mosh
        mtr
        nmap
        #packer
        tcpdump
        telnet
        wakelan
        wget
      ] ++ (if !pkgs.stdenv.isDarwin then [
        bind    # (for dig) - and dig is included with osx
        tcpdump # broken on darwin
        telnet  # broken on darwin
      ] else []);
    };

    benleyDevTools = with pkgs; buildEnv {
      name = "benleyDevTools";
      paths = [
        bundler
        ctags
        diffutils
        gitFull
        gitAndTools.hub
        haskellPackages.ShellCheck
        html-tidy
        nix-repl
        nix-serve
        nix-prefetch-scripts
        nixpkgs-lint
        nodePackages.js-yaml
        nodePackages.jshint
        #nodePackages.npm2nix
        #rbtools
        #ruby
        #silver-searcher
        tig
      ];
    };

    benleySystemTools = with pkgs; buildEnv {
      name = "benleySystemTools";
      ignoreCollisions = true;
      paths = [
        bashInteractive
        bashCompletion
        (lib.lowPrio coreutils)  # lowPrio to resolve conflict with procps
        file
        dstat  # linux only
        gnugrep
        htop
        iftop  # linux only
        iotop  # linux only
        less
        lsof
        #man_db
        ncurses  # for terminfo
        #openssh # system ssh is fine
        procps   # ps kill top free w watch uptime vmstat ...
        pv
        rsync
        socat
        sysstat
        tree
        which
      ];
    };

    benleyPythonDev = with pkgs; buildEnv {
      name = "benleyPythonDev";
      paths = [
        pythonPackages.pylint
        pythonFull
        pythonPackages.autopep8
        pythonPackages.flake8
        pythonPackages.pep8
        pythonPackages.virtualenv
        pythonPackages.virtualenvwrapper
      ];
    };

    benleyGuiStuff = with pkgs; buildEnv {
      name = "benleyGuiStuff";
      paths = [
        myChromium
        firefoxWrapper
        gnome3.gnome-disk-utility
        gparted
      ];
    };

    benleyDesktop = with pkgs; buildEnv {
      name = "benleyDesktop";
      paths = [
        dmenu
        gnupg
        i3
        i3status
        keychain
        powerline-fonts
        redshift
        #gnome3.gnome_terminal
        #(writeTextFile {
        #  name = "benleyEnv";
        #  destination = "/etc/profile.d/benleyEnv";
        #  text = ''
        #    export TERMINAL=gnome-terminal
        #  '';
        #})
      ];
    };

    benleyHaskellDev = pkgs.buildEnv {
      name = "benleyHaskellDev";
      paths = with pkgs; [
        myGhc
        haskellPackages.hindent
        (writeTextFile {
          name = "ghc-shellhook";
          executable = true;
          destination = "/etc/profile.d/nix-ghc.sh";
          text = ''
            # nixpkgs does some hilarious tricks to make haskell libraries work
            eval "$(egrep ^export "${myGhc}/bin/ghc")"
          '';
        })
      ];
    };

    myGhc = haskellPackages.ghcWithHoogle
      (haskellPackages: with haskellPackages; [
        # libs
        aeson
        cryptohash
        file-embed
        megaparsec
        parsec
        megaparsec
        split
        text
        turtle
        unique
        # tools
        cabal-install
        ghc-mod
        hdevtools
        hlint
      ]);

    myChromium = chromiumBeta.override { enableNaCl = true; };

    myScripts = stdenv.mkDerivation {
      name = "benley-scripts";
      src = ./scripts;
      installPhase = ''
        mkdir -p $out/bin
        cp -a $src/* $out/bin/
      '';
    };

    # Enable kerberos in the default openssh package so it gets included with
    # things like git
    #openssh = openssh.override {
    #  withKerberos = true;
    #  withGssapiPatches = true;
    #};

  # end packageOverrides
  };
}
