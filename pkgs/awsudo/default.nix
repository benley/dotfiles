{ stdenv, buildPythonPackage, fetchFromGitHub, boto, retrying, awscli }:

buildPythonPackage rec {
  pname = "awsudo";
  version = "2018-02-06";

  src = fetchFromGitHub {
    owner = "makethunder";
    repo = "awsudo";
    rev = "c128a375b531e4d594c3674a3d71d2e89221ebea";
    sha256 = "1wpk6r76kwglj5jzj28l7cbns48b38x6xm6nazc82xcd6f17ahxa";
  };

  patches = [
    ./awscli-version.patch
  ];

  propagatedBuildInputs = [
    boto
    retrying
    awscli
  ];

  meta = with stdenv.lib; {
    homepage = https://github.com/makethunder/awsudo;
    description = "sudo-like utility to manage AWS credentials";
    # license = ????;
    maintainers = with maintainers; [ benley ];
  };
}
