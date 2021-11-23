self: super:

with rec {
  myEmacs = super.emacs.override {
    # withGTK3 = false;  # fucking X11 disconnect crash bug...
    # withGTK2 = false;
    imagemagick = self.imagemagick;  # why isn't this enabled by default?
    # withXwidgets = true;  # Cool, but I haven't found much of a use for it
  };

  emacsWithPackages = (self.emacsPackagesNgGen myEmacs).emacsWithPackages;
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
    # bazel-mode  # No longer in nixpkgs?
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
    # erlang  # fails to build? (2021-05-26)
    flx-ido
    flycheck
    flycheck-package
    flycheck-pos-tip
    # flycheck-color-mode-line  # not needed with doom-modeline
    # flycheck-status-emoji  # not needed with doom-modeline
    forge
    form-feed
    git-gutter
    git-modes
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
    org-roam
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
  ])

  );
}
