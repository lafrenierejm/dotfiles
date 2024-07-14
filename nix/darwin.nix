{
  inputs,
  config,
  lib,
  pkgs,
  personal,
  hostname,
  userName,
  ...
}: let
  dotnetBrew = "dotnet@6";
  opensslBrew = "openssl";
  libraries =
    [dotnetBrew opensslBrew]
    ++ (lib.lists.optionals (!personal) ["postgresql@12"]);
  brewPath = pkg: "/opt/homebrew/opt/${pkg}";
in (lib.attrsets.mergeAttrsList [
  rec {
    nix-homebrew = {
      enable = true;
      enableRosetta = false;
      user = userName;
      taps = {
        "hashicorp/homebrew-tap" = inputs.homebrew-hashicorp;
        "homebrew/homebrew-bundle" = inputs.homebrew-bundle;
        "homebrew/homebrew-cask" = inputs.homebrew-cask;
        "homebrew/homebrew-core" = inputs.homebrew-core;
      };
      mutableTaps = false;
    };
    homebrew = {
      enable = true;
      global = {
        autoUpdate = false;
      };
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
      casks = let
        electron = ["slack"];
        greedy = map (name: {
          inherit name;
          greedy = true;
        }) (lib.lists.optionals personal ["iina"]);
      in
        lib.lists.flatten [
          electron
          greedy
          "amazon-chime"
          "displaylink"
          "lunar"
          "scroll-reverser"
          "zoom"
          (lib.lists.optionals personal [
            "aldente"
            "balenaetcher"
            "eloston-chromium"
            "gog-galaxy"
            "inkscape"
            "multipatch"
            "openemu"
            "radio-silence"
            "skype"
            "snes9x"
            "steam"
            "tailscale"
            "transmission"
            "visualboyadvance-m"
            "zsa-wally"
          ])
          (lib.lists.optionals (!personal) [
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
          Kindle = 302584613;
          Parcel = 639968404;
        })
      ];
      taps = builtins.attrNames nix-homebrew.taps;
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
      CustomUserPreferences = {
        "com.colliderli.iina".SUEnableAutomaticChecks = false;
      };
      NSGlobalDomain = {
        "com.apple.mouse.tapBehavior" = 1; # tap to click
        AppleInterfaceStyleSwitchesAutomatically = true;
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
  (
    lib.attrsets.optionalAttrs personal
    {
      networking = {
        hostName = hostname;
        knownNetworkServices = [
          "Thunderbolt Bridge"
          "USB 10/100/1000 LAN"
          "Wi-Fi"
          "iPhone USB"
        ];
      };
    }
  )
])
