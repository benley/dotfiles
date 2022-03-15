{ python3Packages, fetchFromGitHub, hddtemp, hdparm, smartmontools }:

let pythonPackages = python3Packages; in

with { inherit (pythonPackages) buildPythonPackage; };

buildPythonPackage rec {
  pname = "hddfancontrol";
  version = "1.4.3";

  src = fetchFromGitHub {
    owner = "desbma";
    repo = pname;
    rev = version;
    sha256 = "0hn0qxwqvbvifzrmyxsr979s67bq1v8m0dvdr6lvsz22ggz85kj6";
  };

  checkInputs = [
    hddtemp
    hdparm
  ];

  propagatedBuildInputs = [
    pythonPackages.python-daemon
    hddtemp
    hdparm
    smartmontools
  ];

  postInstall = ''
    mkdir -p $out/etc/systemd/system
    substitute systemd/hddfancontrol.service $out/etc/systemd/system/hddfancontrol.service \
        --replace /usr/bin/hddfancontrol $out/bin/hddfancontrol
    sed -i -e '/EnvironmentFile=.*/d' $out/etc/systemd/system/hddfancontrol.service
  '';
}
