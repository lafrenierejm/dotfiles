{
  inputs,
  outputs,
  config,
  lib,
  pkgs,
  personal,
  system,
  userName,
  ...
}: let
  fontPackages = with pkgs; [source-code-pro font-awesome];
in {
  age.secrets.cachix-auth.file = ./cachix-auth.age;
  nix = {
    enable = true;

    # Add each flake input as a registry to make `nix` subcommands consistent with the flake.
    registry = lib.mapAttrs (_: value: {flake = value;}) inputs;
    # Add flake inputs to the system's legacy channels to make legacy `nix-*` commands consistent.
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;

    package = pkgs.nixVersions.stable;
    settings =
      {
        experimental-features = "nix-command flakes";
        substituters = [
          "https://nix-community.cachix.org"
          "https://lafrenierejm.cachix.org"
        ];
        trusted-public-keys = [
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          "lafrenierejm.cachix.org-1:80p6+l8ziffNuhGRtiZu0xsV5FGXk2GbkOU2unIi8OM="
        ];
        trusted-users = [userName];
      }
      // lib.attrsets.optionalAttrs personal {
        post-build-hook = pkgs.writeScript "cachix-push" ''
          #!/bin/sh
          set -eu
          set -f # disable globbing
          export IFS=' '
          export CACHIX_AUTH_TOKEN="$(cat ${config.age.secrets.cachix-auth.path})"
          exec ${pkgs.cachix}/bin/cachix push lafrenierejm $OUT_PATHS
        '';
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
      coreutils
      curl
      exiftool
      fuc
      fd
      file
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
