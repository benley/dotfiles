{ stdenv
, lib
, fetchurl
, makeWrapper
, dpkg
, glibc
, glib
, libglvnd
, libxcb
, libxkbcommon
, libXcomposite
, libXcursor
, libXi
, libXrandr
, libXtst
, libX11
, libXrender
, alsaLib
, nss
, nspr
, qt5
, xdg_utils
, zlib
, fontconfig
}:

let rpath = lib.makeLibraryPath [
  alsaLib
  fontconfig
  glibc
  glib
  libxkbcommon
  libX11
  libxcb
  libXrender
  libXcomposite
  libXi
  libXrandr
  libXtst
  libglvnd
  nss
  nspr
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
  version = "3.0.19.40421";
  src =
    if stdenv.hostPlatform.system == "x86_64-linux" then
      fetchurl {
        url = "http://s.insynchq.com/builds/${name}_${version}-bionic_amd64.deb";
        sha256 = "06yia565ad10b7wblj2av09wixhz0hqvyagi3hc3xz1s20vqyslh";
      }
    else
      throw "${name} is not supported on ${stdenv.hostPlatform.system}";


  buildInputs = [ dpkg makeWrapper ];

  unpackPhase = "dpkg-deb --fsys-tarfile $src | tar -x --no-same-permissions --no-same-owner";

  installPhase = ''
    mkdir -p $out/usr
    cp -R usr/ $out/
    ln -s $out/usr/share $out/share
    # chmod a-x $out/usr/lib/insync/library.zip # do not match in the next loop

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
