{ stdenv
, lib
, fetchurl
, makeWrapper
, dpkg
, glibc
, glib
, libxcb
, libGL
, nss
, libthai
, wayland
, alsaLib
, qt5
, autoPatchelfHook
}:

stdenv.mkDerivation rec {
  name = "insync";
  version = "3.2.4.40856";
  src =
    if stdenv.hostPlatform.system == "x86_64-linux" then
      fetchurl {
        url = "http://s.insynchq.com/builds/${name}_${version}-focal_amd64.deb";
        sha256 = "1bvqbbrfn5784nmb2qaflm1rzczqhvghhb6y5zaxrapyhygxbcis";
      }
    else throw "${name} is not supported on ${stdenv.hostPlatform.system}";

  buildInputs = [
    libxcb
    libGL
    nss libthai wayland alsaLib
    qt5.qtvirtualkeyboard
    qt5.qtwebchannel
    qt5.qtwebsockets
    qt5.qtlocation
    qt5.qtwebengine
  ];

  nativeBuildInputs = [ autoPatchelfHook dpkg makeWrapper qt5.wrapQtAppsHook ];

  unpackPhase = ''
    dpkg-deb --fsys-tarfile $src | tar -x --no-same-permissions --no-same-owner
  '';

  installPhase = ''
    mkdir -p $out/bin $out/lib $out/share
    cp -R usr/* $out/
    rm $out/lib/insync/libGLX.so.0
    rm $out/lib/insync/libQt5*
    sed -i 's|/usr/lib/insync|/lib/insync|' "$out/bin/insync"
    wrapQtApp "$out/lib/insync/insync"
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
