self: super:

with rec {
  myEmacs = super.emacs.override {
    withGTK3 = false;  # fucking X11 disconnect crash bug...
    withGTK2 = false;
    imagemagick = self.imagemagick;  # why isn't this enabled by default?
    # withXwidgets = true;  # Cool, but I haven't found much of a use for it
  };

  emacsWithPackages = (self.emacsPackagesNgGen myEmacs).emacsWithPackages;

  udev-mode = {
    pname = "udev-mode";
    version = "20200626.0";
    src = super.fetchFromGitHub {
      owner = "benley";
      repo = "emacs-udev-mode";
      rev = "e939a83712e9d5b95f7315945806dbfa0ef36244";
      sha256 = "1jqsn0f6qf7znna62z20ynzawc1v9nxlb6krwp371n51x367c2bj";
    };
    recipe = super.writeText "recipe" ''
      (udev-mode :fetcher github :repo "benley/emacs-udev-mode")
    '';
    packageRequires = [];
  };

  my-jsonnet-mode = {
    pname = "jsonnet-mode";
    version = "20190322.0";
    src = super.fetchFromGitHub {
      owner = "benley";
      repo = "jsonnet-mode";
      rev = "03fb75f63b33b3a69ac35a189d9dca93c0a01871";
      sha256 = "1wr6wyq31dwr518yfhfa0f5z8qzx27lmm16gf9jm3vz5d5pqi22j";
    };
    recipe = super.writeText "recipe" ''
      (jsonnet-mode :fetcher github :repo "mgyucht/jsonnet-mode")
    '';
    packageRequires = [];
  };

  ox-ipynb = {
    pname = "ox-ipynb";
    version = "20190104.0";
    src = super.fetchFromGitHub {
      owner = "jkitchin";
      repo = "ox-ipynb";
      rev = "1aa8f28cedc7c328ddf47798c2628a1cbdbeffe8";
      sha256 = "1wf72y1d3q3fwxhmbjdfdy4yh6lx5jv37cfiwr7j6r8fv5cwbvl7";
    };
    recipe = super.writeText "recipe" ''
      (ox-ipynb :fetcher github :repo "jkitchin/ox-ipynb")
    '';
  };
};

{
  texlive-for-orgmode = super.texlive.combine {
    inherit (super.texlive) scheme-small wrapfig capt-of cm-super dvipng;
  };

  basicEmacs = super.emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
    json-mode
    nix-mode
    smex
    use-package
    yaml-mode
  ]));

  fancyEmacs = emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [

  ]) ++ (with epkgs.melpaPackages; [
    all-the-icons
    arduino-mode
    atomic-chrome
    # base16-theme
    bazel-mode
    centaur-tabs
    company
    company-box
    # company-emoji
    company-nixos-options
    company-posframe
    company-terraform
    cython-mode
    diminish
    # docker
    dockerfile-mode
    doom-modeline
    doom-themes
    edit-indirect
    # emojify
    esup
    evil
    erlang
    flx-ido
    flycheck
    flycheck-package
    flycheck-pos-tip
    flycheck-color-mode-line
    flycheck-status-emoji
    forge
    form-feed
    git-gutter
    gitattributes-mode
    gitconfig-mode
    gitignore-mode
    go-mode
    graphviz-dot-mode
    haskell-mode
    # highlight-indentation
    highlight-indent-guides
    htmlize
    idle-highlight-mode
    epkgs.melpaPackages."ido-completing-read+"
    ido-grid-mode
    imenu-list
    json-mode
    # jsonnet-mode  # local copy
    jq-mode
    lsp-mode
    lsp-haskell
    lsp-ui
    lsp-treemacs
    magit
    magit-popup
    markdown-mode
    # material-theme
    nix-mode
    nix-sandbox
    # nyan-mode
    org-bullets
    org-jira
    org-journal
    org-make-toc
    # org-pdfview  # abandoned
    org-sticky-header
    ox-asciidoc
    ox-gfm
    ox-rst
    package-lint
    paredit
    powerline
    projectile
    protobuf-mode
    quelpa-use-package
    rainbow-delimiters
    solaire-mode
    ssh-config-mode
    smex
    smooth-scrolling
    # spaceline-all-the-icons
    # spaceline
    # spacemacs-theme
    # slime
    # stumpwm-mode
    systemd
    tabbar
    terminal-here
    terraform-mode
    treemacs
    treemacs-magit
    treemacs-projectile
    use-package
    vdiff
    vdiff-magit
    vimrc-mode
    visual-fill-column
    w3m
    web-mode
    # weechat  # someday I'll start using this probably
    which-key
    ws-butler
    xterm-color
    yaml-mode

  ]) ++ (with epkgs.elpaPackages; [

    delight
    exwm

  ]) ++ (with epkgs.orgPackages; [

    org

  ]) ++ (with epkgs; [
    vterm
    pdf-tools
  ] ++ [

    (epkgs.melpaBuild my-jsonnet-mode)
    (epkgs.melpaBuild ox-ipynb)
    (epkgs.melpaBuild udev-mode)

  ])

  );
}
