{ stdenv, fetchurl }:

let version = "1.7.5"; in

stdenv.mkDerivation {
  name = "kubernetes-client-${version}";
  srcs = fetchurl {
    url = "https://dl.k8s.io/v${version}/kubernetes-client-linux-amd64.tar.gz";
    sha256 = "1b8p0n17s7ndpbjl5rpav05sjcfwmfb4d7gpqp8smk71gcsxqip2";
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
