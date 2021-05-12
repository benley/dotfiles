{ config, pkgs, lib, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "bstaffin";
  home.homeDirectory = "/home/bstaffin";

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
    azure-cli
    mu
    kops_1_18
    kubectl
    stern
    (aws-google-auth.override { withU2F = true; })
    nodePackages.bash-language-server
    jsonnet
    shellcheck
    yq
    kubernetes-helm
    kubecfg
    sshuttle
    minikube
    whois
    curlie
    dmenu
    j4-dmenu-desktop
    gopls
  ];

  home.sessionVariables = {
    XDG_DATA_DIRS = "$HOME/.nix-profile/share:$XDG_DATA_DIRS";
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
    enableNixDirenvIntegration = true;
  };

  fonts.fontconfig.enable = true;

  # finish this later
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs.use-package
      epkgs.magit
      epkgs.org-plus-contrib
      epkgs.vterm
    ];
  };

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
        extraConfig = "set -g @tmux_power_theme 'sky'";
      }
    ];
    extraConfig = builtins.readFile /home/bstaffin/p/dotfiles/cfg/.tmux.conf;
  };

  services.emacs = {
    enable = true;
    client.enable = true;
  };

  xsession.enable = true;
  xsession.preferStatusNotifierItems = true;
  # xsession.windowManager.command = lib.mkForce ''
  #   export KDEWM=xmonad
  #   xterm&
  #   /usr/bin/startkde
  # '';

  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
  };
}
