{ pythonPackages }:

pythonPackages.buildPythonApplication {
  name = "yaml2json";
  srcs = ./yaml2json.py;
}
