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
    arcanist
    azure-cli
    cachix
    element-desktop
    # mu  # maildir thing to use with emacs mh maybe?
    kubectl
    stern
    nodePackages.bash-language-server
    haskell-language-server
    jsonnet
    shellcheck
    yq
    kubernetes-helm
    # kubecfg
    # sshuttle
    # minikube
    whois
    curlie
    # dmenu
    # j4-dmenu-desktop
    gopls
    zoom-us
    nix-top
  ];

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
      # less: extended status prompt, display ANSI colors, case-insensitive search.
      LESS = "-M -R -i";
      # nix: don't automatically pipe output to $PAGER, it's too distracting
      NIX_PAGER = "";
    };
    initExtra = ''
      source ${pkgs.gitAndTools.gitFull}/share/git/contrib/completion/git-prompt.sh
      for file in ${./bashrc.d}/*; do
        source "$file"
      done
      export PYTHONSTARTUP=${builtins.path {path=./cfg/.pythonrc.py; name="pythonrc.py";}};
    '';
  };

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };

  # fonts.fontconfig.enable = true;

  # finish this later
  # programs.emacs = {
  #   enable = true;
  #   extraPackages = epkgs: [
  #     epkgs.use-package
  #     epkgs.magit
  #     epkgs.org-plus-contrib
  #     epkgs.vterm
  #     epkgs.json-mode
  #   ];
  # };

  programs.git = {
    enable = true;
    userName = "Benjamin Staffin";
    userEmail = "bstaffin@singlestore.com";

    aliases = {
      st = "status";
	    lg = ''log --graph --pretty=format:'%Cblue%h%Creset %Cgreen%an%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative'';
      uplog = "lg ..@{u}";
    };

    lfs.enable = true;

    signing.key = "FDBA38EE781A69D439A870A5A490C0134E09AF4A";

    ignores = [
      ''\#*\#''  # emacs turds
    ];

    extraConfig = {
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
    };

    includes = [{path = "~/.config/git/secret-stuff.config";}];
  };

  programs.tmux = {
    enable = true;
    aggressiveResize = true;
    terminal = "tmux-24bit";
    historyLimit = 50000;
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
    extraConfig = builtins.readFile ./cfg/.tmux.conf;
  };

  # services.emacs = {
  #   enable = true;
  #   client.enable = true;
  # };

  xresources.properties = {
    "Xft.dpi" = 144;
  };

  # xsession.enable = true;
  # xsession.preferStatusNotifierItems = true;

  # xsession.windowManager.xmonad = {
  #   enable = true;
  #   enableContribAndExtras = true;
  # };

  # services.taffybar.enable = true;

  systemd.user.sockets.emacs = {
    Unit = {
      Description = "Emacs: the extensible, self-documenting text editor";
      Documentation = "info:emacs man:emacs(1) https://gnu.org/software/emacs/";
    };
    Socket = {
      ListenStream = "%t/emacs/server";
      FileDescriptorName = "server";
      SocketMode = "0600";
      DirectoryMode = "0700";
    };
    Install = {
      WantedBy = ["sockets.target"];
    };
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
