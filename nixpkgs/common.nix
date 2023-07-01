{ inputs, outputs, config, lib, pkgs, ... }: {
  nix = {
    # # This will add each flake input as a registry
    # # To make nix3 commands consistent with your flake
    # registry = lib.mapAttrs (_: value: { flake = value; }) inputs;

    # # This will additionally add your inputs to the system's legacy channels
    # # Making legacy nix commands consistent as well, awesome!
    # nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;

    package = pkgs.nixVersions.stable;
    settings = { experimental-features = "nix-command flakes"; };
  };
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [ atool exa fd git ripgrep zsh ];

  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [ source-code-pro font-awesome ];
  };

  programs = { zsh.enable = true; };
}
