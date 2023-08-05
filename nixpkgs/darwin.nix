{ config, lib, pkgs, personal, ... }:

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
    casks =
      [ "karabiner-elements" "logitech-unifying" "lunar" "scroll-reverser" ]
      ++ (lib.lists.optionals personal [
        "aldente"
        "balenaetcher"
        "bitwarden"
        "iina"
        "mullvadvpn"
        "multipatch"
        "openemu"
        "snes9x"
        "steam"
        "transmission"
        "visualboyadvance-m"
        "yt-music"
      ]);
    taps = [ "homebrew/cask-drivers" "railwaycat/emacsmacport" ];
  };

  services = { nix-daemon.enable = true; };

  system.keyboard.enableKeyMapping = true; # needed for skhd
  environment.systemPath =
    [ "${dotnetInstallDir}/bin" config.homebrew.brewPrefix ];
  environment.variables = { DOTNET_ROOT = "${dotnetInstallDir}/libexec"; };
}
