{ stdenv, ncurses, lib }:

# https://www.gnu.org/software/emacs/manual/html_mono/efaq.html#Colors-on-a-TTY

stdenv.mkDerivation rec {
  name = "terminfo-extras";
  srcs = lib.cleanSource ./.;

  buildInputs = [ ncurses ];

  installPhase = ''
    install -dm 755 "$out/share/terminfo"
    tic -s -x -o "$out/share/terminfo" 24bit.terminfo
    runHook postInstall
  '';
}
