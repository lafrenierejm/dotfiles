{
  inputs,
  outputs,
  config,
  lib,
  pkgs,
  system,
  userName,
  ...
}: let
  fontPackages = with pkgs; [source-code-pro font-awesome];
in {
  nix = {
    # # This will add each flake input as a registry
    # # To make nix3 commands consistent with your flake
    # registry = lib.mapAttrs (_: value: { flake = value; }) inputs;

    # # This will additionally add your inputs to the system's legacy channels
    # # Making legacy nix commands consistent as well, awesome!
    # nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;
    package = pkgs.nixVersions.stable;
    settings = {
      experimental-features = "nix-command flakes";
      substituters = ["https://cachix.org/api/v1/cache/emacs"];
      trusted-public-keys = ["emacs.cachix.org-1:b1SMJNLY/mZF6GxQE+eDBeps7WnkT0Po55TAyzwOxTY="];
      trusted-users = [userName];
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

  environment.systemPackages = [inputs.agenix.packages."${system}".default] ++ (with pkgs; [atool fd git ripgrep zsh]);

  fonts =
    {
      packages = with pkgs; [
        font-awesome
        noto-fonts
        noto-fonts-cjk
        noto-fonts-emoji
        source-code-pro
        source-han-sans
        source-han-sans-japanese
        source-han-serif-japanese
      ];
    }
    // (lib.attrsets.optionalAttrs pkgs.stdenv.isLinux {
      fontconfig.defaultFonts = {
        serif = [
          "Noto Serif"
          "Source Han Serif"
        ];
        sansSerif = [
          "Noto Sans"
          "Source Han Sans"
        ];
      };
      fontDir.enable = true;
    });

  programs = {
    zsh.enable = true;
  };
}
