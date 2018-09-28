{ stdenv, python3Packages, fetchFromGitHub,
  libappindicator-gtk3, libnotify, wrapGAppsHook,
  gobjectIntrospection }:

python3Packages.buildPythonPackage rec {
  pname = "battery-monitor";
  version = "2018-09-04";

  srcs = fetchFromGitHub {
    owner = "maateen";
    repo = "battery-monitor";
    rev = "42d02b288980e338798eb1483ae127864aea1984";
    sha256 = "07rpjr8f72dqqi4mkk5bs05r1jgqzb4lngf8rqd2qbksqf2kfis3";
  };

  propagatedBuildInputs = with python3Packages; [
    pygobject3
  ];

  buildInputs = [
    libappindicator-gtk3
    libnotify
    wrapGAppsHook
  ];

  nativeBuildInputs = [
    gobjectIntrospection
  ];

  postBuild = ''
    substitute battery-monitor.desktop.in battery-monitor.desktop \
               --replace "%EXEC_PATH%" "$out/bin/battery-monitor" \
               --replace "%ICON_PATH%" "$out/share/pixmaps/battery-monitor.png"
    substitute battery-monitor-autostart.desktop.in battery-monitor-autostart.desktop \
               --replace "%EXEC_PATH%" "$out/bin/battery-monitor" \
               --replace "%ICON_PATH%" "$out/share/pixmaps/battery-monitor.png" \
  '';

  postInstall = ''
    mkdir -p $out/share/pixmaps
    cp battery_monitor/icons/icon.png $out/share/pixmaps/battery-monitor.png
    mkdir -p $out/etc/xdg/autostart
    cp battery-monitor-autostart.desktop $out/etc/xdg/autostart/
    mkdir -p $out/share/applications
    cp battery-monitor.desktop $out/share/applications/
  '';
}
