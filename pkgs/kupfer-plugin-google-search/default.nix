{ stdenv }:

stdenv.mkDerivation {
  name = "kupfer-plugin-google-search";

  srcs = ./kupfer;

  installPhase = ''
    mkdir -p $out/share/kupfer
    cp -a plugins $out/share/kupfer/plugins
  '';
}
