{ python3Packages, fetchFromGitHub, hddtemp, hdparm, smartmontools }:

let pythonPackages = python3Packages; in

with { inherit (pythonPackages) buildPythonPackage; };

buildPythonPackage rec {
  pname = "hddfancontrol";
  version = "1.3.1";

  src = fetchFromGitHub {
    owner = "desbma";
    repo = pname;
    rev = version;
    sha256 = "0h2cazi9as6q7bx88zv6mh26kmn2nwcyzxsnwg1bqvm0i2qzkkkx";
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
