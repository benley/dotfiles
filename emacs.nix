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
diminish
#emojify
flycheck
flycheck-pos-tip
flycheck-color-mode-line
gitignore-mode
go-mode
haskell-mode
ido-completing-read-plus
jsonnet-mode
magit
markdown-mode
#material-theme
nix-mode
nyan-mode
org-bullets
paredit
protobuf-mode
rainbow-delimiters
smart-mode-line
smex
#spaceline-all-the-icons
#spaceline
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