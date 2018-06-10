{ python3Packages, fetchFromGitHub, hddtemp, hdparm }:

let pythonPackages = python3Packages; in

with { inherit (pythonPackages) buildPythonPackage; };

buildPythonPackage rec {
  pname = "hddfancontrol";
  version = "1.2.8";

  src = fetchFromGitHub {
    owner = "desbma";
    repo = pname;
    rev = version;
    sha256 = "1k50wny65hdm1nk5pzfcrhfka90xw2p7z475hy7sinblcb1zrl2r";
  };

  doCheck = false;

  preBuild = ''
    echo "removed" > README.md
    sed -i 's/2.1.2/2.1.1/' requirements.txt
  '';

  propagatedBuildInputs = [
    pythonPackages.pythondaemon
    hddtemp
    hdparm
  ];
}
