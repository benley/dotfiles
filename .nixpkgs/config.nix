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
        bc
        benleyDesktop
        benleyDevTools
        #benleyGuiStuff
        benleyHaskellDev
        benleyNetTools
        benleyPythonDev
        benleySystemTools
        jq
        myScripts
        myVim
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
        bind    # there isn't a separate package for "dig" yet
        httpie
        mosh
        mtr
        nmap
        #packer
        tcpdump
        telnet
        wget
      ];
    };

    benleyDevTools = with pkgs; buildEnv {
      name = "benleyDevTools";
      paths = [
        cabal2nix
        ctags
        diffutils
        gitFull
        gitAndTools.hub
        haskellPackages.ShellCheck
        honcho
        html-tidy
        nix-repl
        nix-prefetch-scripts
        nixpkgs-lint
        nodePackages.js-yaml
        nodePackages.jshint
        nodePackages.npm2nix
        #rbtools
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
        pylint
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

    myGhc = haskellPackages.ghcWithPackages (x: with x; [
      aeson
      text
      ghc-mod
      hdevtools
      hlint
      hoogle
    ]);

    myChromium = chromiumBeta.override { enableNaCl = true; };

    myVim = vim_configurable.customize {
      name = "vim";
      vimrcConfig = {
        customRC = builtins.readFile ./vimrc;
        vam.knownPlugins = pkgs.vimPlugins;
        vam.pluginDictionaries = [
          { names = [
              "Solarized"
              "Supertab"
              "Syntastic"
              "Tagbar"
              "The_NERD_tree"
              "fugitive"
              "ghcmod"
              "neco-ghc"
              "rainbow_parentheses"
              "taglist"
              "vim2hs"
              "vim-addon-nix"
              "vim-airline"
              "vim-coffee-script"
              "vim-gitgutter"
              #"vim-hdevtools"
              "vimproc"
              "vimshell-vim"
              "youcompleteme"
            ];
          }
        ];
      };
    };

    myScripts = stdenv.mkDerivation {
      name = "benley-scripts";
      src = ./scripts;
      installPhase = ''
        mkdir -p $out/bin
        cp -a $src/* $out/bin/
      '';
    };
  };
}
