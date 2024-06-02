{
  inputs,
  config,
  lib,
  pkgs,
  personal,
  ...
}: let
  dotnetBrew = "dotnet@6";
  opensslBrew = "openssl";
  libraries =
    [dotnetBrew opensslBrew]
    ++ (lib.lists.optionals (!personal) ["postgresql@12"]);
  brewPath = pkg: "/opt/homebrew/opt/${pkg}";
in {
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      upgrade = true;
      cleanup = "uninstall";
    };
    brews =
      ["make" "nvm" "nuget" "pyenv"]
      ++ libraries
      ++ (
        lib.lists.optionals (!personal) ["hashicorp/tap/boundary"]
      );
    casks =
      [
        "displaylink"
        "eloston-chromium"
        "lunar"
        "scroll-reverser"
        "zoom"
      ]
      ++ (lib.lists.optionals personal [
        "aldente"
        "balenaetcher"
        "bitwarden"
        "gog-galaxy"
        "iina"
        "inkscape"
        "mullvadvpn"
        "multipatch"
        "openemu"
        "radio-silence"
        "skype"
        "snes9x"
        "steam"
        "transmission"
        "visualboyadvance-m"
        "zsa-wally"
      ])
      ++ (lib.lists.optionals (!personal) [
        "amazon-chime"
        "docker"
        "firefox"
      ]);
    taps =
      ["homebrew/cask-drivers"]
      ++ (
        lib.lists.optionals (!personal) ["hashicorp/tap"]
      );
  };
  environment = {
    systemPath =
      lib.trivial.concat [config.homebrew.brewPrefix]
      (map (pkg: (brewPath pkg) + "/bin") libraries);
    variables = rec {
      CFLAGS =
        lib.concatStringsSep " "
        (map (pkg: ("-I" + (brewPath pkg) + "/include")) libraries);
      CPPFLAGS = CFLAGS;
      DOTNET_ROOT = (brewPath dotnetBrew) + "/libexec";
      LDFLAGS =
        lib.concatStringsSep " "
        (map (pkg: ("-L" + (brewPath pkg) + "/lib")) libraries);
      OPENSSL_DIR = brewPath opensslBrew;
      OPENSSL_ROOT_DIR = OPENSSL_DIR;
      PKG_CONFIG_PATH =
        lib.concatStringsSep " "
        (map (pkg: (brewPath pkg) + "/lib/pkgconfig") libraries);
    };
  };

  # Whether Touch ID is enabled as a `sudo` auth mechanism.
  security.pam.enableSudoTouchIdAuth = personal;
  services = {
    karabiner-elements.enable = true;
    nix-daemon.enable = true;
  };
}
