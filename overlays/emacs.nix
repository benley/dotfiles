self: super:

with rec {
  myEmacs = super.emacs.override {
    withGTK3 = true;
    withGTK2 = false;
  };

  emacsWithPackages = (self.emacsPackagesNgGen myEmacs).emacsWithPackages;

};

{
  emacs = emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [

  ]) ++ (with epkgs.melpaPackages; [

    base16-theme
    bazel-mode
    company
    # company-emoji
    company-terraform
    diminish
    docker
    edit-indirect
    elscreen
    # emojify
    evil
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
    kubernetes
    magit
    magit-gh-pulls
    magit-popup  # a dep of magit, but I want the newer-than-melpaStable version
    markdown-mode
    # material-theme
    nix-mode
    nix-sandbox
    # nyan-mode
    org-bullets
    org-jira
    org-journal
    paredit
    powerline
    protobuf-mode
    rainbow-delimiters
    # slack
    # smart-mode-line
    smex
    smooth-scrolling
    # spaceline-all-the-icons
    # spaceline
    terminal-here
    terraform-mode
    treemacs
    use-package
    vdiff
    vdiff-magit
    web-mode
    weechat
    xterm-color
    yaml-mode

  ]) ++ (with epkgs.elpaPackages; [

    exwm

  ]) ++ (with epkgs.orgPackages; [

    org

  ]) ++ (with epkgs; [
    emacs-libvterm
  ])

  );
}
