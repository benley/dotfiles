{ stdenv, fetchzip }:

stdenv.mkDerivation {
  pname = "libdatrie";
  version = "0.2.12";

  srcs = fetchzip {
    url = "https://github.com/tlwg/libdatrie/releases/download/v0.2.12/libdatrie-0.2.12.tar.xz";
    sha256 = "188p54da841waz3wla89l58vdvk4rr6mqlc13bq2m7gy4dlpgh8v";
  };
}
