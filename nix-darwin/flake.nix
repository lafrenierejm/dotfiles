# cd ~
# nix build ~/.config/nix-darwin\#darwinConfigurations.macbook.system
# ./result/sw/bin/darwin-rebuild switch --flake ~/.config/nix-darwin

{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, darwin }:
    let
      configuration = { lib, pkgs, ... }: {
        nix.package = pkgs.nixVersions.stable;
        nixpkgs.config.allowUnfree = true;

        environment.systemPackages = with pkgs; [
          atool
          babashka
          bitwarden-cli
          clojure
          direnv
          exa
          fd
          ghq
          git
          git-crypt
          nix-direnv
          nixfmt
          nodePackages.prettier
          ripgrep
          rnix-lsp
          rsync
          rust-analyzer
          unrar
          zsh
        ];

        programs = {
          zsh.enable = true;
        };

        services = {
          nix-daemon.enable = true;
          skhd = {
            enable = true;
            package = pkgs.skhd;
            skhdConfig = lib.concatStringsSep "\n" [
              # Note that `alt` is Option.

              # Window control
              "alt - f : yabai -m window --toggle native-fullscreen"

              # Focus window
              "alt - k : yabai -m window --focus north"
              "alt - j : yabai -m window --focus south"
              "alt - h : yabai -m window --focus west"
              "alt - l : yabai -m window --focus east"

              # Swap windows
              "shift + alt - k : yabai -m window --swap north"
              "shift + alt - j : yabai -m window --swap south"
              "shift + alt - h : yabai -m window --swap west"
              "shift + alt - l : yabai -m window --swap east"

              # Focus space
              "alt - 1 : yabai -m space --focus 1"
              "alt - 2 : yabai -m space --focus 2"
              "alt - 3 : yabai -m space --focus 3"
              "alt - 4 : yabai -m space --focus 4"
              "alt - 5 : yabai -m space --focus 5"
              "alt - 6 : yabai -m space --focus 6"
              "alt - 7 : yabai -m space --focus 7"
              "alt - 8 : yabai -m space --focus 8"
              "alt - 9 : yabai -m space --focus 9"

              # Send to space
              "shift + alt - 1 : yabai -m window --space 1"
              "shift + alt - 2 : yabai -m window --space 2"
              "shift + alt - 3 : yabai -m window --space 3"
              "shift + alt - 4 : yabai -m window --space 4"
              "shift + alt - 5 : yabai -m window --space 5"
              "shift + alt - 6 : yabai -m window --space 6"
              "shift + alt - 7 : yabai -m window --space 7"
              "shift + alt - 8 : yabai -m window --space 8"
              "shift + alt - 9 : yabai -m window --space 9"
              "shift + alt - right : yabai -m window --space next"
              "shift + alt - left : yabai -m window --space prev"
            ];
          };
          yabai = {
            enable = true;
            package = pkgs.yabai;
            config = {
              layout = "bsp";
              auto_balance = "on";
              window_border = "on";
              window_border_placement = "inset";
              window_border_width = 1;
            };
          };
        };

        system.keyboard.enableKeyMapping = true; # needed for skhd
      };
    in
    {
    darwinConfigurations = {
      macbook = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [ configuration ];
      };
    };

    # Expose the package set, including overlays, for convenience.
    darwinPackages = self.darwinConfigurations.apple-silicon.pkgs;
  };
}
