{ stdenv, python, fetchFromGitHub, makeWrapper }:

stdenv.mkDerivation rec {
  version = "0.3.2";
  name = "nix-home-${version}";

  src = fetchFromGitHub {
    rev = "f84fc9059bd3c7aec403c032748715eb3a2d2b95";
    repo = "nix-home";
    owner = "benley";
    sha256 = "0ad8rnhwy6na9yw5a6rqprcrmjv2wgjg4djn37dqwf3fq2vg1z8w";
  };

  # src = fetchFromGitHub {
  #   rev = version;
  #   repo = "nix-home";
  #   owner = "sheenobu";
  #   sha256 = "0l27vg651s9mmq0sypxgrrdq9386rhjbgh9wilzm3dmr0d2j9mwa";
  # };

  buildInputs = [ makeWrapper ];

  dontStrip = true;     # it's just scripts
  dontPatchELF = true;  # it's just scripts

  installPhase = ''
    # install binary
    mkdir -p $out/bin
    cp $src/nix-home.sh $out/bin/nix-home
    cp $src/nix-build-home.sh $out/bin/nix-build-home
    wrapProgram $out/bin/nix-home       --set NIXHOME $out/nix/lib
    wrapProgram $out/bin/nix-build-home --set NIXHOME $out/nix/lib
    mkdir -p $out/nix
    cp -a lib $out/nix
  '';

  meta = {
    homepage = https://github.com/sheenobu/nix-home;
    description = "Per-user configuration management via Nix";
    license = stdenv.lib.licenses.mit;
    platforms = stdenv.lib.platforms.unix;
    inherit version;
  };
}
