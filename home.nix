{ config, pkgs, lib, ... }:

{
  nixpkgs.config = import ./nixpkgs-config.nix;
  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;
  nixpkgs.overlays = import ./nixpkgs-overlays.nix;

  ###
  # Set these if _not_ using home-manager as a nixos system module:
  # programs.home-manager.enable = true;
  # home.username = "benley";
  # home.homeDirectory = "/home/benley";
  ###

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.09";

  home.packages = with pkgs; [
    actionlint
    # azure-cli
    # mu  # maildir thing to use with emacs mh maybe?
    awscli2
    blackbox
    cabal-install
    cabal2nix
    cachix
    copilot-language-server-fhs
    fd  # used by doom emacs (project file search)
    ctags
    curlie
    diffstat  # tanka wants this (TODO: fix upstream)
    editorconfig-core-c
    fd  # emacs projectile uses this
    go
    google-cloud-sdk
    google-drive-ocamlfuse
    gopls
    haskell-language-server
    html-tidy
    imagemagick  # so emacs can resize images
    jsonnet
    jsonnet-language-server
    krew
    kube3d
    kubectl
    kubelogin-oidc
    nixfmt-classic
    nix-top
    nodePackages.bash-language-server
    nodePackages.js-yaml
    nodePackages.jshint
    pyright
    pandoc
    pdfarranger
    ripgrep
    shellcheck
    sqlite  # org-roam uses this
    sshfs
    stack
    stern
    tanka
    whois
    xournalpp
    yq
  ];

  dconf.settings = {
    "org/gnome/mutter" = {
      "experimental-features" = ["scale-monitor-framebuffer"];
    };
  };

  gtk = {
    enable = false;
    cursorTheme.name = "Nordic-cursors";
    cursorTheme.package = pkgs.nordic;
    # cursorTheme.size = 16;
    iconTheme.name = "Nordic-green";
    iconTheme.package = pkgs.nordic;
    theme.name = "Nordic";
    theme.package = pkgs.nordic;
    # font.name = "Noto Sans Condensed";
    # font.size = 10;
    # font.package = ...
  };

  programs.bash = {
    enable = true;
    enableVteIntegration = true;
    historyControl = ["ignoredups" "ignorespace"];
    shellOptions = [
      "histappend"
      "checkwinsize"  # update LINES and COLUMNS after each command
      "globstar"      # extended ** globbing (different from extglob!)
      "checkjobs"
      "execfail"      # don't exit an interactive shell if "exec blah" fails
    ];
    sessionVariables = {
      # Fancy timestamps in .bash_history
      HISTTIMEFORMAT = "%Y-%m-%d %T ";
      # less: extended status prompt, display ANSI colors, case-insensitive search, colorful status bar
      LESS = "-M -R -i --use-color";
      # nix: don't automatically pipe output to $PAGER, it's too distracting
      # NIX_PAGER = "";
    };
    initExtra = ''
      source ${pkgs.gitAndTools.gitFull}/share/git/contrib/completion/git-prompt.sh
      for file in ${./bashrc.d}/*; do
        source "$file"
      done
      export PYTHONSTARTUP=${builtins.path {path=./cfg/.pythonrc.py; name="pythonrc.py";}};

      # https://felipec.wordpress.com/2021/06/05/adventures-with-man-color/
      export MANPAGER="less -M -R -i --use-color -Dd+R -Du+B -DHkC -j5"
      export MANROFFOPT="-c"  # unclear if this does anything on nixos
    '';
  };

  home.sessionPath = [
    "$HOME/.krew/bin"
  ];

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };

  # fonts.fontconfig.enable = true;

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      # Preinstall some non-trivial dependencies so emacs doesn't need to build
      # them from source
      epkgs.vterm
      epkgs.emacsql
      epkgs.treesit-grammars.with-all-grammars
    ];
    package = pkgs.emacs-pgtk;
  };

  services.darkman = {
    enable = true;
    settings.usegeoclue = true;
    darkModeScripts = {
      gtk3-theme = ''
        gsettings set org.gnome.desktop.interface gtk-theme Adwaita-dark
        gsettings set org.gnome.desktop.interface color-scheme prefer-dark
      '';
    };
    lightModeScripts = {
      gtk3-theme = ''
        gsettings set org.gnome.desktop.interface gtk-theme Adwaita
        gsettings set org.gnome.desktop.interface color-scheme default
      '';
    };
  };

  services.emacs = {
    enable = true;
    client.enable = true;
    defaultEditor = true;
    socketActivation.enable = true;
  };

  home.file = {
    ".doom.d" = {
      recursive = true;
      source = ./cfg/.doom.d;
    };
  };

  programs.git = {
    enable = true;

    lfs.enable = true;

    signing.key = "FDBA38EE781A69D439A870A5A490C0134E09AF4A";

    ignores = [
      ''\#*\#''  # emacs turds
    ];

    settings = {
      user.name = "Benjamin Staffin";
      user.email = "benley@gmail.com";

      aliases = {
        st = "status";
        lg = ''log --graph --pretty=format:'%Cblue%h%Creset %Cgreen%an%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative'';
        uplog = "lg ..@{u}";
      };

      init.defaultBranch = "main";

      merge.dpkg-changelogs = {
        name = "debian/changelog merge driver";
        driver = "dpkg-mergechangelogs -m %O %A %B %A";
      };
      rerere.enabled = true;
      credential.helper = "libsecret";
      github.user = "benley";
      pull.rebase = true;
      gitlab = {
        "code.xkrd.net/api/v4".user = "benley";
        "gitlab.cloud.memcompute.com/api/v4".user = "bstaffin";
      };
      push.default = "nothing";

      user.useConfigOnly = true;

      http.cookieFile = "~/.config/git/cookies";
      # http.saveCookies = true;

      diff.blackbox.textconv = "gpg --use-agent -q --batch --decrypt";

      url."git@gitlab.com:" = {
        insteadOf = "https://gitlab.com/";
      };
    };

    includes = [{path = "~/.config/git/secret-stuff.config";}];
  };

  programs.gh = {
    enable = true;
    settings = {
      aliases = {
        co = "pr checkout";
      };
      git_protocol = "https";
      prompt = "enabled";
    };
  };

  programs.rbw = {
    enable = true;
    settings = {
      email = "benley@zoiks.net";
      pinentry = pkgs.pinentry-gnome3;
      base_url = "https://vault.zoiks.net";
      lock_timeout = 3600;
      sync_interval = 3600;
    };
  };

  programs.tmux = {
    enable = true;
    aggressiveResize = true;
    historyLimit = 50000;
    keyMode = "vi";
    mouse = true;
    plugins = [
      {
        plugin = pkgs.tmuxPlugins.power-theme;
        extraConfig = ''
          set -g @tmux_power_theme 'sky'
          set -g @tmux_power_user_icon ''
          set -g @tmux_power_time_icon ''
          set -g @tmux_power_session_icon ''
        '';
      }
    ];
    extraConfig = ''
      # pane movement
      bind-key J command-prompt -p "join pane from:"  "join-pane -s ':%%'"
      #bind-key s command-prompt -p "send pane to:"  "join-pane -t ':%%'"

      # Open new windows with the same cwd as the current one
      bind-key c new-window -c '#{pane_current_path}'
      bind-key '"' split-window -c '#{pane_current_path}'
      bind-key % split-window -h -c '#{pane_current_path}'

      # don't be anal retentive about releasing ^B before hitting another key
      bind-key C-n next-window
      bind-key C-p previous-window
      bind-key C-c new-window -c '#{pane_current_path}'

      set -ga terminal-overrides ',xterm*:XT:Ms=\E]52;%p1%s;%p2%s\007'
      set -ga terminal-overrides ',screen*:XT:Ms=\E]52;%p1%s;%p2%s\007'
    '';
  };

  programs.readline = {
    enable = true;
    # There are some other options here (bindings, variables) but they
    # don't seem to be better than just using extraConfig at this
    # point, so I'll just include my existing inpurc file
    extraConfig = builtins.readFile ./cfg/.inputrc;
    includeSystemConfig = true;
  };
}
