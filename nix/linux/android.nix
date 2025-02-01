{pkgs, ...}: {
  programs.adb.enable = true;
  services.udev.packages = with pkgs; [
    android-udev-rules
  ];
}
