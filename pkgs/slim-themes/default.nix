{ stdenv, lib, slimThemes, imagemagick,
  randrHeads ? 1
}:

{
  # NixOS theme with a dark background and white text
  nixos-dark = stdenv.mkDerivation {
    name = "slim-theme-nixos-dark";
    src = slimThemes.nixosSlim;
    phases = [ "unpackPhase" "patchPhase" "installPhase" ];
    patchPhase = let
      headFactor = if randrHeads == [] then 1 else lib.length randrHeads;
      centerLeft = 100 / (headFactor * 2);
    in ''
      ${imagemagick.out}/bin/mogrify \
        -fill '#080010' -draw 'color 0,0 reset' \
        share/slim/themes/nixos-slim-testing/background.png
      ${imagemagick.out}/bin/mogrify \
        -negate -region 100x110+0+0 -negate -fill white -colorize 20% \
        share/slim/themes/nixos-slim-testing/panel.png
      sed -i \
        -e 's/^\([a-z_]\+_x[^0-9]*\)[0-9]\+%/\1${toString centerLeft}%/' \
        share/slim/themes/nixos-slim-testing/slim.theme
      cat >> share/slim/themes/nixos-slim-testing/slim.theme <<EOF
      session_x      ${toString centerLeft}%
      msg_color      #ffffff
      username_color #ffffff
      password_color #ffffff
      input_color    #ffffff
      EOF
    '';
    installPhase = ''
      cp -R share/slim/themes/nixos-slim-testing "$out"
    '';
  };
}
