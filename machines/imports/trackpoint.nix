{ config, pkgs, ... }:

{
  # Bind the trackpoint device to use the libinput driver instead of evdev
  services.xserver.inputClassSections = [
    ''
      Identifier "Trackpoint"
      MatchProduct "TPPS/2 IBM TrackPoint"
      MatchDevicePath "/dev/input/event*"
      Driver "libinput"
    ''
  ];
}
