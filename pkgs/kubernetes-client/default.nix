{ stdenv, fetchurl }:

let version = "1.6.3"; in

stdenv.mkDerivation {
  name = "kubernetes-client-${version}";
  srcs = fetchurl {
    url = "https://dl.k8s.io/v1.6.3/kubernetes-client-linux-amd64.tar.gz";
    sha256 = "0xbymz6flay4v3gs9jsc5j0c2qndv10vif5md129r20is061w55f";
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
