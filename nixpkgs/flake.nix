# nix build .#darwinConfigurations.macbook.system
# ./result/sw/bin/darwin-rebuild switch --flake .

{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs = {
      url = "github:cmacrae/emacs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    spacebar.url = "github:cmacrae/spacebar/v1.4.0";
  };

  outputs =
    { self, nixpkgs, darwin, emacs, home-manager, spacebar, ... }@inputs:
    let
      configuration = { lib, pkgs, ... }: {
        nix = {
          extraOptions = "experimental-features = nix-command flakes";
          package = pkgs.nixVersions.stable;
          settings = {
            substituters = [ "https://cachix.org/api/v1/cache/emacs" ];
            trusted-public-keys = [
              "emacs.cachix.org-1:b1SMJNLY/mZF6GxQE+eDBeps7WnkT0Po55TAyzwOxTY="
            ];
          };
        };

        nixpkgs = {
          config.allowUnfree = true;
          overlays = [ emacs.overlay ];
        };

        environment.systemPackages = with pkgs; [
          atool
          exa
          fd
          git
          ripgrep
          zsh
        ];

        fonts = {
          fontDir.enable = true;
          fonts = with pkgs; [ source-code-pro font-awesome ];
        };

        programs = { zsh.enable = true; };

        services.nix-daemon.enable = true;

      };
    in {
      darwinConfigurations = {
        macbook = darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          modules = [
            configuration
            ./darwin.nix
            home-manager.darwinModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.lafrenierejm = import ./home.nix;
            }
          ];
        };
      };

      # Expose the package set, including overlays, for convenience.
      darwinPackages = self.darwinConfigurations.apple-silicon.pkgs;
    };
}
