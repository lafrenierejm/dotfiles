{ config, lib, pkgs, ... }:

let
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
    brews = [ "emacs-mac" "nuget" "pyenv" dotnetPackage ];
    casks = [
      "aldente"
      "balenaetcher"
      "bitwarden"
      "firefox"
      "iina"
      "karabiner-elements"
      "logitech-unifying"
      "lunar"
      "mullvadvpn"
      "multipatch"
      "openemu"
      "scroll-reverser"
      "snes9x"
      "steam"
      "transmission"
      "visualboyadvance-m"
      "yt-music"
    ];
    taps = [ "homebrew/cask-drivers" "railwaycat/emacsmacport" ];
  };

  services = { nix-daemon.enable = true; };

  system.keyboard.enableKeyMapping = true; # needed for skhd
  environment.systemPath =
    [ "${dotnetInstallDir}/bin" config.homebrew.brewPrefix ];
  environment.variables = { DOTNET_ROOT = "${dotnetInstallDir}/libexec"; };
}
