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
        benleyDevTools
        #benleyGuiStuff
        #benleyHaskellDev
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
        bind    # there isn't a separate package for "dig" yet
        httpie
        mosh
        mtr
        nmap
        #packer
        tcpdump
        telnet
      ];
    };

    benleyDevTools = with pkgs; buildEnv {
      name = "benleyDevTools";
      paths = [
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
        dstat
        gnugrep
        htop
        iftop
        iotop
        less
        lsof
        #man_db
        ncurses  # for terminfo
        openssh
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
        python
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
        i3
        i3status
        dmenu
        powerline-fonts
      ];
    };

    benleyHaskellDev = pkgs.buildEnv {
      name = "benleyHaskellDev";
      paths = with haskellPackages; [
        ghc
        hdevtools
        hlint
        hoogle
        # Whythehell does this not work with ghc 7.10.whatever?
        haskell.packages.ghc784.ghc-mod
      ];
    };

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
              "rainbow_parentheses"
              "taglist"
              "vim-addon-nix"
              "vim-airline"
              "vim-coffee-script"
              "vim-gitgutter"
              "vimproc"
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
