{ stdenv
, boost
, cmake
, fetchFromGitHub
, pkgconfig
, txt2tags
, udev
}:

stdenv.mkDerivation rec {
  name = "thunderbolt-${version}";
  version = "0.9.3";
  src = fetchFromGitHub {
    owner = "01org";
    repo = "thunderbolt-software-user-space";
    rev = "v${version}";
    sha256 = "02w1bfm7xvq0dzkhwqiq0camkzz9kvciyhnsis61c8vzp39cwx0x";
  };

  patches = [
    ./tbtacl-shellcheck.patch
  ];

  buildInputs = [
    boost
    cmake
    pkgconfig
    txt2tags
  ];

  # These can't go in the normal nix cmakeFlags because $out needs to be
  # expanded by the shell, not by cmake or nix.  $ENV{out} doesn't work right
  # either; it results in /build/source/build//nix/store/blahblahblahblah/bin/
  preConfigure = ''
    cmakeFlags+=" -DUDEV_BIN_DIR=$out/bin"
    cmakeFlags+=" -DUDEV_RULES_DIR=$out/etc/udev/rules.d"
  '';

  meta = {
    description = "Thunderbolt(TM) user-space components";
    license = stdenv.lib.licenses.bsd3;
    maintainers = [ stdenv.lib.maintainers.ryantrinkle ];
    homepage = https://01.org/thunderbolt-sw;
    platforms = stdenv.lib.platforms.linux;
  };
}
