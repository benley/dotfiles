import <nixpkgs> {
  overlays = [
    (import ./overlays)
    (import ./overlays/emacs.nix)
  ];
}
