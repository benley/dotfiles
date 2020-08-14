{ stdenv, fetchzip
, pkgconfig
, libdatrie
}:

let version = "0.1.28"; in

stdenv.mkDerivation {
  pname = "libthai";
  inherit version;

  srcs = fetchzip {
    url = "https://github.com/tlwg/libthai/releases/download/v0.1.28/libthai-0.1.28.tar.xz";
    sha256 = "0savv5if9bkgc1qpav4jl6j7pa1bd8x7pr5wk537260ggjpfcdcy";
  };

  nativeBuildInputs = [
    pkgconfig
  ];

  buildInputs = [
    libdatrie
  ];
}
