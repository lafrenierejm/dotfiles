{
  inputs,
  config,
  lib,
  pkgs,
  personal,
  system,
  ...
}: let
  dotnetPackage = "dotnet@6";
  dotnetInstallDir = "/opt/homebrew/opt/${dotnetPackage}";
in {
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      upgrade = true;
      cleanup = "uninstall";
    };
    brews = ["libusb" "nuget" "pyenv" dotnetPackage];
    casks =
      ["karabiner-elements" "logitech-unifying" "lunar" "scroll-reverser"]
      ++ (lib.lists.optionals personal [
        "aldente"
        "balenaetcher"
        "bitwarden"
        "iina"
        "mullvadvpn"
        "multipatch"
        "openemu"
        "skype"
        "snes9x"
        "steam"
        "transmission"
        "visualboyadvance-m"
        "yt-music"
        "zsa-wally"
      ]);
    taps = ["homebrew/cask-drivers"];
  };

  services = {nix-daemon.enable = true;};

  # Whether Touch ID is enabled as a `sudo` auth mechanism.
  security.pam.enableSudoTouchIdAuth = personal;

  environment.pathsToLink = ["/share/bash-completion"];
  environment.systemPath = ["${dotnetInstallDir}/bin" config.homebrew.brewPrefix];
  environment.variables = {DOTNET_ROOT = "${dotnetInstallDir}/libexec";};
}
