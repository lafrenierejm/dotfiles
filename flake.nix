{
  description = "Joseph LaFreniere (lafrenierejm)'s dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";
    nixpkgs-firefox.url = github:Enzime/nixpkgs/firefox-bin-darwin;
    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = github:lafrenierejm/emacs-overlay/feature/aarch64-darwin;
    emacs-src = {
      url = github:emacs-mirror/emacs/emacs-29;
      flake = false;
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-root.url = "github:srid/flake-root";
    gron.url = "github:lafrenierejm/gron";
    home-manager = {
      url = github:lafrenierejm/home-manager/release-23.05_ripgrep-all;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pre-commit-hooks-nix = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    ripgrep-all = {
      url = github:lafrenierejm/ripgrep-all;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    ripsecrets = {
      url = "github:sirwart/ripsecrets";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        inputs.flake-root.flakeModule
        inputs.pre-commit-hooks-nix.flakeModule
        inputs.treefmt-nix.flakeModule
      ];
      systems = ["x86_64-linux" "aarch64-darwin"];
      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: {
        # Per-system attributes can be defined here. The self' and inputs'
        # module parameters provide easy access to attributes of the same
        # system.
        packages =
          {
            default = pkgs.hello;
          }
          // pkgs.lib.optionalAttrs (system == "x84_64-linux") {
            install-iso = inputs.nixos-generators.nixosGenerate {
              inherit system;
              format = "install-iso";
            };
          };

        # Auto formatters. This also adds a flake check to ensure that the
        # source tree was auto formatted.
        treefmt.config = {
          projectRootFile = ".git/config";
          package = pkgs.treefmt;
          flakeCheck = false; # use pre-commit's check instead
          programs = {
            alejandra.enable = true; # nix
            shellcheck.enable = true;
            shfmt = {
              enable = true;
              indent_size = null;
            };
            prettier.enable = true;
          };
        };

        pre-commit = {
          check.enable = true;
          settings.hooks = {
            editorconfig-checker.enable = true;
            treefmt.enable = true;
            typos.enable = true;
          };
        };

        devShells.default = pkgs.mkShell {
          # Inherit all of the pre-commit hooks.
          inputsFrom = [config.pre-commit.devShell];
        };
      };

      flake = {
        # The usual flake attributes can be defined here, including system-
        # agnostic ones like nixosModule and system-enumerating ones, although
        # those are more easily expressed in perSystem.
        darwinConfigurations = {
          macbook = let
            username = "lafrenierejm";
          in
            inputs.darwin.lib.darwinSystem rec {
              system = "aarch64-darwin";
              modules = [
                ./nix/common.nix
                ./nix/darwin.nix
                inputs.home-manager.darwinModules.home-manager
                {
                  nixpkgs.overlays = [
                    inputs.emacs-overlay.overlays.default
                  ];
                  home-manager.extraSpecialArgs = {
                    inherit inputs system username;
                    gitEmail = "git@lafreniere.xyz";
                    gitUseGpg = true;
                  };
                  home-manager.useGlobalPkgs = true;
                  home-manager.useUserPackages = true;
                  home-manager.users."${username}" = import ./nix/home.nix;
                  users.users."${username}".home = "/Users/${username}";
                }
              ];
              specialArgs = {
                inherit inputs system;
                personal = true;
              };
            };
        };
        nixosConfigurations = {
          earthbound = let
            username = "lafrenierejm";
            system = "x86_64-linux";
          in
            inputs.nixpkgs.lib.nixosSystem {
              system = "x86_64-linux";
              modules = [
                ./nix/common.nix
                ./nix/earthbound/configuration.nix
                inputs.home-manager.nixosModules.home-manager
                {
                  nixpkgs.overlays = [
                    inputs.emacs-overlay.overlays.default
                  ];
                  home-manager.extraSpecialArgs = {
                    inherit inputs system username;
                    gitEmail = "git@lafreniere.xyz";
                    gitUseGpg = true;
                  };
                  home-manager.useGlobalPkgs = true;
                  home-manager.useUserPackages = true;
                  home-manager.users."${username}" = import ./nix/home.nix;
                  users.users."${username}" = {
                    home = "/home/${username}";
                    openssh.authorizedKeys.keys = [
                      (builtins.readFile ./ssh/macbook.pub)
                    ];
                  };
                }
              ];
              specialArgs = {inherit inputs;};
            };
        };
      };
    };
}
