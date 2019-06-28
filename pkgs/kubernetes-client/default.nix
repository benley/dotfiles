{ stdenv, fetchurl }:

let version = "1.11.5"; in

stdenv.mkDerivation {
  name = "kubernetes-client-${version}";
  srcs = fetchurl {
    url = "https://dl.k8s.io/v${version}/kubernetes-client-linux-amd64.tar.gz";
    sha256 = "1s1ynbh27vwadyb4y3nzixfb15akd5iri91b67d4ja1b9v24q65p";
  };

  installPhase = ''
    mkdir -p $out/bin
    cp client/bin/* $out/bin/

    mkdir -p $out/share/bash-completion/completions
    mkdir -p $out/share/zsh/site-functions
    $out/bin/kubectl completion bash > $out/share/bash-completion/completions/kubectl
    $out/bin/kubectl completion zsh > $out/share/zsh/site-functions/_kubectl
  '';
}
