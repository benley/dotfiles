{ writeTextFile }:

writeTextFile {
  name = "adafruit-udev-rules";
  text = builtins.readFile ./adafruit.rules;
  destination = "/etc/udev/rules.d/70-adafruit.rules";
}
