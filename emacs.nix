{ pkgs? import <nixpkgs> {} }:

with rec {
  myEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
};

emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [

]) ++ (with epkgs.melpaPackages; [

base16-theme
company
#company-emoji
company-terraform
diminish
elscreen
#emojify
flycheck
flycheck-pos-tip
flycheck-color-mode-line
flycheck-status-emoji
git-gutter
gitignore-mode
go-mode
haskell-mode
ido-completing-read-plus
json-mode
jsonnet-mode
magit
magit-popup  # a dep of magit, but I want the newer-than-melpaStable version
markdown-mode
#material-theme
nix-mode
nix-sandbox
#nyan-mode
org-bullets
paredit
powerline
protobuf-mode
rainbow-delimiters
#smart-mode-line
smex
#spaceline-all-the-icons
#spaceline
terraform-mode
treemacs
use-package
web-mode
yaml-mode

]) ++ (with epkgs.elpaPackages; [

]) ++ (with epkgs.orgPackages; [

org

]) ++ (with epkgs; [

])

)
