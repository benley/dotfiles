{ stdenv, libpulseaudio, pkgconfig, libnotify, gdk-pixbuf, fetchFromGitHub }:

stdenv.mkDerivation rec {
  name = "powermate-linux-0.5+git2018-02-16";

  srcs = fetchFromGitHub {
    owner = "stefansundin";
    repo = "powermate-linux";
    rev = "c6ff4c138f2c652f7feb714a40e33242efaf2527";
    sha256 = "0r96s0fdk4c9ily1xvppv23h7kxy8qhnbvyc06q3h6xbg5pf8fsa";
  };

  patches = [
    ./dont-daemonize.patch
    ./udev.patch
    ./be-quiet.patch
  ];

  buildInputs = [
    libpulseaudio
    pkgconfig
    libnotify
    gdk-pixbuf
  ];

  installPhase = ''
    mkdir -p $out/bin $out/etc/udev/rules.d
    cp powermate $out/bin/powermate
    cp 60-powermate.rules $out/etc/udev/rules.d
  '';
}
