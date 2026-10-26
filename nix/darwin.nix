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
  caBundlePath = "/etc/nix/ca-bundle.crt";

  # macOS keys Full Disk Access (TCC) grants to an app's bundle ID and code-signing
  # requirement, but Home Manager's `Emacs.app` lives at a store path and is re-signed
  # on every rebuild, so a grant made to it goes stale.
  # Instead, grant Full Disk Access once to this small launcher, which has a fixed
  # path and a fixed designated requirement (see the activation script below).
  # It spawns (rather than execs) Emacs so that TCC keeps attributing Emacs's file
  # accesses to the launcher as the responsible process.
  # See `pkgs/emacs-launcher`.
  emacsLauncherId = "xyz.lafreniere.emacs-launcher";
  emacsLauncherApp = "/Applications/Emacs Launcher.app";
  emacsLauncher = pkgs.callPackage ./pkgs/emacs-launcher {
    bundleId = emacsLauncherId;
    # The per-user profile does not link `Applications/`, so point at the package.
    # A new Emacs build rebuilds the launcher, which is fine because the TCC grant
    # is matched by the signing requirement rather than the binary.
    emacsPath = "${config.home-manager.users.${userName}.programs.emacs.finalPackage}/Applications/Emacs.app/Contents/MacOS/Emacs";
  };

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
      alt-tab = "com.lwouis.alt-tab-macos";
      fmail3 = "fr.appmac.fmail3";
      iina = "com.colliderli.iina";
      lunar = "fyi.lunar.Lunar";
      multipatch = "com.sappharad.MultiPatch";
      openemu = "org.openemu.OpenEmu";
      syncthing-app = "com.github.xor-gate.syncthing-macosx";
      tailscale-app = "io.tailscale.ipn.macsys";
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
      "font-dejavu"
      "font-dejavu-sans"
      "font-noto-emoji"
      "font-noto-mono"
      "font-noto-sans"
      "font-noto-sans-mono"
      "font-noto-sans-symbols"
      "font-noto-sans-symbols-2"
      "font-source-code-pro"
      "karabiner-elements"
      "ungoogled-chromium"
      "zoom"
      (lib.lists.optionals personal [
        "apparency"
        "gog-galaxy"
        "inkscape"
        "microsoft-word"
        "netnewswire"
        "radio-silence"
        "snes9x"
        "steam"
        "visualboyadvance-m"
      ])
      (lib.lists.optionals (!personal) [
        "docker-desktop"
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
        "homebrew/homebrew-cask" = inputs.homebrew-cask;
        "homebrew/homebrew-core" = inputs.homebrew-core;
      };
      mutableTaps = false;
    };
    homebrew = {
      inherit casks;
      enable = true;
      global.autoUpdate = false;
      onActivation.cleanup = "uninstall";
      brews = lib.lists.flatten [
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
          Manet-Music = 6470928235;
          Parcel = 375589283; # Parcel-Classic = 639968404;
          Whisper-Transcription = 1668083311;
        })
      ];
      taps = builtins.attrNames nix-homebrew.taps;
    };
    environment.systemPath = [config.homebrew.prefix];
    ids.gids.nixbld = 30000;

    nix.settings.ssl-cert-file = caBundlePath;
    launchd.daemons."org.nixos.nix-daemon".serviceConfig.EnvironmentVariables = {
      GIT_SSL_CAINFO = caBundlePath;
      NIX_SSL_CERT_FILE = caBundlePath;
    };

    security.pam.services.sudo_local.touchIdAuth = personal;
    services = {
      skhd.enable = personal;
      yabai = {
        enable = personal;
        enableScriptingAddition = true;
      };
    };
    system.activationScripts.postActivation.text = lib.mkAfter ''
      # Build a CA bundle that includes System keychain certificates for Nix.
      mkdir -p /etc/nix
      cat ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt > ${caBundlePath}
      /usr/bin/security find-certificate -a -p /Library/Keychains/System.keychain >> ${caBundlePath} 2>/dev/null || true

      # Install the Emacs launcher (see `emacsLauncher`) at a fixed path.
      # The explicit designated requirement keeps the TCC grant valid when the
      # launcher binary changes; Full Disk Access must still be granted to it once,
      # manually, in System Settings > Privacy & Security.
      launcher=${lib.escapeShellArg emacsLauncherApp}
      if [ "$(cat "$launcher/Contents/Resources/store-path" 2>/dev/null)" != ${emacsLauncher} ]; then
        rm -rf "$launcher"
        cp -R "${emacsLauncher}/Applications/Emacs Launcher.app" "$launcher"
        chmod -R u+w "$launcher"
        mkdir -p "$launcher/Contents/Resources"
        echo ${emacsLauncher} > "$launcher/Contents/Resources/store-path"
        /usr/bin/codesign --force --sign - \
          --identifier ${emacsLauncherId} \
          -r='designated => identifier "${emacsLauncherId}"' \
          "$launcher"
      fi
    '';
    system.stateVersion = 5;
    system.primaryUser = userName;
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
