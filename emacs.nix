{ pkgs ? import <nixpkgs> {} }:

with rec {
  myEmacs = pkgs.emacs;

  myEmacs26 = pkgs.emacs.overrideAttrs (oldAttrs: rec {
    name = "emacs-${version}";
    version = "26.0.91";

    src = pkgs.fetchFromGitHub {
      owner = "emacs-mirror";
      repo = "emacs";
      rev = name;
      sha256 = "0gp8jp7x857bsdgaw4r535qaspq65p0w8wznrdl8wvygp8sgbymr";
    };
  });
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
};

emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
]) ++ (with epkgs.melpaPackages; [

base16-theme
bazel-mode
company
#company-emoji
company-terraform
diminish
elscreen
#emojify
evil
flycheck
flycheck-pos-tip
flycheck-color-mode-line
flycheck-status-emoji
git-gutter
gitignore-mode
go-mode
haskell-mode
htmlize
ido-completing-read-plus
ido-grid-mode
json-mode
# jsonnet-mode  # local copy
magit
magit-popup  # a dep of magit, but I want the newer-than-melpaStable version
markdown-mode
#material-theme
nix-mode
nix-sandbox
#nyan-mode
org-bullets
org-jira
paredit
powerline
protobuf-mode
rainbow-delimiters
#smart-mode-line
smex
smooth-scrolling
#spaceline-all-the-icons
#spaceline
terraform-mode
treemacs
use-package
web-mode
yaml-mode

]) ++ (with epkgs.elpaPackages; [
exwm
]) ++ (with epkgs.orgPackages; [

org

]) ++ (with epkgs; [

])

)
