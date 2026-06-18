{
  inputs,
  config,
  lib,
  pkgs,
  personal,
  system,
  userName,
  ...
}: {
  age.secrets.cachix-auth = {
    file = ./secrets/cachix-auth.age;
    mode = "440";
    owner = "root";
    group = "nixbld";
  };
  nix = {
    enable = true;

    # Add registry entries for nixpkgs and nur.
    # This allows for commands like `nix run -- nur#package`.
    registry = {
      nixpkgs.flake = inputs.nixpkgs;
      nur.flake = inputs.nur;
    };
    # Add the registry keys to system's legacy channels to make legacy `nix-*` commands consistent.
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;

    package = pkgs.nixVersions.stable;
    settings =
      {
        auto-optimise-store = true; # detect and replace identical files in store with hard links
        experimental-features = "nix-command flakes";
        substituters = [
          "https://nix-community.cachix.org"
          "https://lafrenierejm.cachix.org"
        ];
        trusted-public-keys = [
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          "lafrenierejm.cachix.org-1:I0CTtIze4k7s1kvHtnP04rMfnmT0qSToc0zMqJ80eNg="
        ];
        trusted-users = [userName];
      }
      // lib.attrsets.optionalAttrs personal {
        post-build-hook = lib.getExe (pkgs.writeShellApplication {
          name = "cachix-push";
          runtimeInputs = with pkgs; [cachix];
          runtimeEnv = {
            CACHIX_AUTH_TOKEN_FILE = config.age.secrets.cachix-auth.path;
          };
          text = builtins.readFile ./cachix-push.sh;
        });
      };

    gc =
      {
        automatic = true;
        options = "--delete-older-than 30d";
      }
      // (lib.attrsets.optionalAttrs pkgs.stdenv.isLinux {
        randomizedDelaySec = "14m";
      });
  };
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = lib.lists.flatten [
    inputs.agenix.packages."${system}".default
    (with pkgs; [
      atool
      btop
      coreutils
      curl
      exiftool
      fd
      file
      findutils # find, xargs, etc.
      fuc # cpz and rmz
      git
      gnupg
      ripgrep
      unzip
      zsh
    ])
  ];
  environment.pathsToLink = [
    "/share/xdg-desktop-portal"
    "/share/applications"
  ];

  programs = {
    zsh.enable = true;
  };
}
