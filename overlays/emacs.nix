self: super:

with rec {
  myEmacs = super.emacs.override {
    withGTK3 = true;
    withGTK2 = false;
    imagemagick = self.imagemagick;  # why isn't this enabled by default?
    # withXwidgets = true;  # Cool, but I haven't found much of a use for it
  };

  emacsWithPackages = (self.emacsPackagesNgGen myEmacs).emacsWithPackages;


  my-jsonnet-mode = {
    pname = "jsonnet-mode";
    version = "20180829.8";
    src = super.fetchFromGitHub {
      owner = "mgyucht";
      repo = "jsonnet-mode";
      rev = "160a35e21163cb88ab91293082afe7f12fc1ecfe";
      sha256 = "1f99s38rn26glp0423isr9x90nmfng3cxgxmc4pw3jx4xh4allfv";
    };
    recipe = super.writeText "recipe" ''
      (jsonnet-mode :fetcher github :repo "mgyucht/jsonnet-mode")
    '';
    packageRequires = [];
  };
};

{
  emacs = emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [

  ]) ++ (with epkgs.melpaPackages; [

    # base16-theme
    bazel-mode
    company
    # company-emoji
    company-terraform
    diminish
    # docker
    edit-indirect
    # elscreen
    # emojify
    evil
    flx-ido
    flycheck
    flycheck-pos-tip
    flycheck-color-mode-line
    flycheck-status-emoji
    git-gutter
    gitignore-mode
    go-mode
    haskell-mode
    highlight-indentation
    htmlize
    idle-highlight-mode
    ido-completing-read-plus
    ido-grid-mode
    json-mode
    # jsonnet-mode  # local copy
    jq-mode
    # kubernetes  # build broken?
    magit
    # magit-gh-pulls  # build broken?
    magit-popup  # a dep of magit, but I want the newer-than-melpaStable version
    markdown-mode
    # material-theme
    nix-mode
    nix-sandbox
    # nyan-mode
    org-bullets
    org-jira
    org-journal
    org-pdfview
    ox-gfm
    paredit
    powerline
    projectile
    protobuf-mode
    rainbow-delimiters
    # slack
    # smart-mode-line
    smex
    smooth-scrolling
    # spaceline-all-the-icons
    # spaceline
    spacemacs-theme
    # slime
    # stumpwm-mode
    tabbar
    terminal-here
    terraform-mode
    treemacs
    treemacs-projectile
    use-package
    vdiff
    vdiff-magit
    vimrc-mode
    web-mode
    # weechat  # someday I'll start using this probably
    which-key
    xterm-color
    yaml-mode

  ]) ++ (with epkgs.elpaPackages; [

    exwm

  ]) ++ (with epkgs.orgPackages; [

    org

  ]) ++ (with epkgs; [
    emacs-libvterm
    pdf-tools
  ] ++ [

    (epkgs.melpaBuild my-jsonnet-mode)

  ])

  );
}
