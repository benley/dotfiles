{ writeTextFile }:

writeTextFile {
  name = "steamcontroller-udev-rules";
  text = builtins.readFile ./steamcontroller.rules;
  destination = "/etc/udev/rules.d/99-steamcontroller.rules";
}
