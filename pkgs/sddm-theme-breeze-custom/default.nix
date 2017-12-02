{ stdenv, plasma-workspace, ... }:

stdenv.mkDerivation {
  name = "sddm-theme-breeze-custom";
  src = plasma-workspace.src;

  patchPhase = ''
    cat > sddm-theme/theme.conf <<EOF
    [General]
    type=image
    background=${./bg.png}
    EOF
  '';

  installPhase = ''
    mkdir -p $out/share/sddm/themes/
    cp -rL sddm-theme $out/share/sddm/themes/breeze-custom
    rm -rf $out/share/sddm/themes/breeze-custom/dummydata
  '';

  propagatedBuildInputs = [
    plasma-workspace
  ];
}
