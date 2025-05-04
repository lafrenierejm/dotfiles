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
  libraries = lib.lists.optionals (!personal) [
    dotnetBrew
    opensslBrew
    "postgresql@12"
  ];
  brewPath = pkg: "/opt/homebrew/opt/${pkg}";

  # Electron-based casks.
  casksElectron = lib.lists.flatten [
    "amazon-chime"
    "balenaetcher"
    "skype"
    (lib.lists.optionals personal [
      "keybase"
      "signal"
      "slack"
    ])
  ];

  # Casks that auto-update using Sparkle.
  casksSparkle = lib.attrsets.mergeAttrsList [
    {
      "scroll-reverser" = "com.pilotmoon.scroll-reverser";
    }
    (lib.attrsets.optionalAttrs personal {
      aldente = "com.apphousekitchen.aldente-pro";
      altserver = "com.rileytestut.AltServer";
      iina = "com.colliderli.iina";
      lunar = "fyi.lunar.Lunar";
      multipatch = "com.sappharad.MultiPatch";
      openemu = "org.openemu.OpenEmu";
      syncthing = "com.github.xor-gate.syncthing-macosx";
      tailscale = "io.tailscale.ipn.macsys";
      transmission = "org.m0k.transmission";
    })
  ];

  # Combined list of casks.
  casks = let
    greedy = map (name: {
      inherit name;
      greedy = true;
    }) (builtins.attrNames casksSparkle);
  in
    lib.lists.flatten [
      casksElectron
      greedy
      "anki"
      "displaylink"
      "firefox"
      "karabiner-elements"
      "zoom"
      (lib.lists.optionals personal [
        "apparency"
        "eloston-chromium"
        "gog-galaxy"
        "inkscape"
        "netnewswire"
        "radio-silence"
        "snes9x"
        "steam"
        "visualboyadvance-m"
      ])
      (lib.lists.optionals (!personal) [
        "docker"
      ])
    ];
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
      inherit casks;
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
        libraries
        (lib.lists.optionals personal ["podman"])
        (lib.lists.optionals (!personal) [
          "hashicorp/tap/boundary"
          "nvm"
          "pyenv"
          "volta"
        ])
      ];
      masApps = lib.attrsets.mergeAttrsList [
        (lib.attrsets.optionalAttrs personal {
          Bitwarden = 1352778147;
          Ivory = 6444602274;
          Kindle = 302584613;
          Parcel = 639968404;
        })
      ];
      taps = builtins.attrNames nix-homebrew.taps;
    };
    environment =
      {
        systemPath = lib.lists.flatten [
          config.homebrew.brewPrefix
          libraries
        ];
      }
      // lib.attrsets.optionalAttrs (!personal) rec {
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
    security.pam.enableSudoTouchIdAuth = personal;
    services = {
      nix-daemon.enable = true;
      skhd.enable = personal;
      yabai = {
        enable = personal;
        enableScriptingAddition = true;
      };
    };
    system.stateVersion = 5;
    system.defaults = {
      CustomUserPreferences =
        lib.attrsets.mergeAttrsList
        (map
          (appId: {
            "${appId}" = {
              SUAutomaticallyUpdate = false;
              SUEnableAutomaticChecks = false;
            };
          })
          (lib.attrsets.attrValues casksSparkle));
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
