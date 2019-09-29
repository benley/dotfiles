{ stdenv
, lib
, fetchurl
, makeWrapper
, dpkg
, glibc
, glib
, libglvnd
, libXcomposite
, libXcursor
, libXi
, libXrandr
, libXtst
, libX11
, libXrender
, nss
, qt5
, xdg_utils
, zlib
, fontconfig
}:

let rpath = lib.makeLibraryPath [
  fontconfig
  glibc
  glib
  libX11
  libXrender
  libXcomposite
  libXi
  libXrandr
  libXtst
  libglvnd
  xdg_utils
  zlib
  qt5.qtbase
  qt5.qtlocation
  qt5.qtdeclarative
  qt5.qtwebengine
  qt5.qtserialport
  qt5.qtwebchannel
];

in stdenv.mkDerivation rec {
  name = "insync";
  version = "1.5.5.37367";
  src =
    if stdenv.hostPlatform.system == "x86_64-linux" then
      fetchurl {
        url = "http://s.insynchq.com/builds/${name}_${version}-artful_amd64.deb";
        sha256 = "1pd2ad3ky0xapcm1ijq1vhv36am62bfb9mrrvbny9x7sanf7ji3w";
      }
    else
      throw "${name} is not supported on ${stdenv.hostPlatform.system}";


  buildInputs = [ dpkg makeWrapper ];

  unpackPhase = "dpkg-deb --fsys-tarfile $src | tar -x --no-same-permissions --no-same-owner";

  installPhase = ''
    mkdir -p $out/usr
    cp -R usr/ $out/
    ln -s $out/usr/share $out/share
    chmod a-x $out/usr/lib/insync/library.zip # do not match in the next loop

    for file in $(find $out -type f \( -perm /0111 -o -name \*.so\* \) ); do
      patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" "$file" || true
      patchelf --set-rpath ${rpath}:$out/usr/lib/insync/ $file || true
    done

    makeWrapper $out/usr/lib/insync/insync $out/bin/insync --set LC_TIME C
  '';

  postPatch = ''
    substituteInPlace usr/bin/insync --replace /usr/lib/insync $out/usr/lib/insync
  '';

  dontConfigure = true;
  dontBuild = true;

  meta = with stdenv.lib; {
    platforms = ["x86_64-linux"];
    license = stdenv.lib.licenses.unfree;
    maintainers = with maintainers; [  ];
    homepage = https://www.insynchq.com;
    description = "Google Drive sync and backup with multiple account support";
    longDescription = ''
     Insync is a commercial application that syncs your Drive files to your
     computer.  It has more advanced features than Google's official client
     such as multiple account support, Google Doc conversion, symlink support,
     and built in sharing.

     There is a 15-day free trial, and it is a paid application after that.
    '';
  };
}
