self: super:

with rec {
  myEmacs = super.emacs.override {
    # withGTK3 = false;  # fucking X11 disconnect crash bug...
    # withGTK2 = false;
    imagemagick = self.imagemagick;  # why isn't this enabled by default?
    # withXwidgets = true;  # Cool, but I haven't found much of a use for it
  };

  emacsWithPackages = (self.emacsPackagesNgGen myEmacs).emacsWithPackages;

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
    inherit (super.texlive) scheme-small wrapfig capt-of cm-super dvipng
    beamertheme-metropolis pgfopts minted fvextra catchfile xstring framed
    # pygmentex
    ;
  };

  basicEmacs = super.emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
    json-mode
    nix-mode
    smex
    use-package
    yaml-mode
  ]));

  fancyEmacs = super.emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [

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
    # flycheck-color-mode-line  # not needed with doom-modeline
    # flycheck-status-emoji  # not needed with doom-modeline
    forge
    form-feed
    git-gutter
    gitattributes-mode
    gitconfig-mode
    gitignore-mode
    go-mode
    graphviz-dot-mode
    haskell-mode
    hide-mode-line
    # highlight-indentation
    highlight-indent-guides
    htmlize
    ibuffer-vc
    idle-highlight-mode
    epkgs.melpaPackages."ido-completing-read+"
    ido-grid-mode
    imenu-list
    json-mode
    jsonnet-mode
    jq-mode
    lsp-mode
    lsp-haskell
    lsp-ui
    lsp-treemacs
    magit
    magit-popup
    markdown-mode
    # material-theme
    nginx-mode
    nix-mode
    nix-sandbox
    # nyan-mode
    org-bullets
    org-jira
    org-journal
    # org-make-toc
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
    # tabbar
    terminal-here
    terraform-mode
    treemacs
    treemacs-magit
    treemacs-projectile
    udev-mode
    use-package
    # vdiff
    # vdiff-magit
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
    # exwm

  ]) ++ (with epkgs.orgPackages; [

    org-plus-contrib

  ]) ++ (with epkgs; [
    vterm
    pdf-tools
  ] ++ [

    (epkgs.melpaBuild ox-ipynb)

  ])

  );
}
