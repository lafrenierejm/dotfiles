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
    brews = lib.lists.flatten [
      "make"
      "nvm"
      "nuget"
      "pyenv"
      libraries
      (lib.lists.optionals (!personal) ["hashicorp/tap/boundary"])
    ];
    casks = lib.lists.flatten [
      "displaylink"
      "eloston-chromium"
      "lunar"
      "scroll-reverser"
      "zoom"
      (lib.lists.optionals personal [
        "aldente"
        "balenaetcher"
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
      (lib.lists.optionals (!personal) [
        "amazon-chime"
        "docker"
        "firefox"
      ])
    ];
    masApps = lib.attrsets.mergeAttrsList [
      {
        Structured = 1499198946;
      }
      (lib.attrsets.optionalAttrs personal {
        Bitwarden = 1352778147;
        Ivory = 6444602274;
        Parcel = 639968404;
      })
    ];
    taps = lib.lists.flatten [
      "homebrew/cask-drivers"
      (lib.lists.optionals (!personal) ["hashicorp/tap"])
    ];
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
  security.pam.enableSudoTouchIdAuth = personal;
  services = {
    karabiner-elements.enable = true;
    nix-daemon.enable = true;
    skhd.enable = personal;
    yabai = {
      enable = personal;
      enableScriptingAddition = true;
    };
  };
  system.defaults = {
    NSGlobalDomain = {
      "com.apple.mouse.tapBehavior" = 1; # tap to click
    };
    dock = {
      autohide = true;
      minimize-to-application = true;
      mru-spaces = false; # do not group spaces by most recent use
      wvous-bl-corner = 1; # disabled
      wvous-br-corner = 1; # disabled
      wvous-tl-corner = 1; # disabled
      wvous-tr-corner = 1; # disabled
    };
    finder = {
      AppleShowAllExtensions = true;
      AppleShowAllFiles = true;
      FXPreferredViewStyle = "Nlsv"; # set default view to list
      ShowPathbar = true; # show breadcrumbs
    };
    menuExtraClock = {
      Show24Hour = true;
      ShowDate = 0; # when space allows
      ShowSeconds = false;
    };
    trackpad = {
      ActuationStrength = 0; # silent clicks
      Clicking = true; # enable tap to click
    };
  };
}
