{
  description = "Joseph LaFreniere (lafrenierejm)'s dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-24.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs";
    agenix.url = "github:ryantm/agenix";
    catppuccin = {
      url = "github:catppuccin/nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane.url = "github:ipetkov/crane";
    darwin = {
      url = "github:lnl7/nix-darwin/nix-darwin-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-plus = {
      url = "github:d12frosted/homebrew-emacs-plus";
      flake = false;
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-root.url = "github:srid/flake-root";
    ghq.url = "github:lafrenierejm/ghq/tilde-nix-flake";
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    gron.url = "github:lafrenierejm/gron";
    homebrew = {
      url = "github:zhaofengli-wip/nix-homebrew";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    homebrew-bundle = {
      url = "github:homebrew/homebrew-bundle";
      flake = false;
    };
    homebrew-core = {
      url = "github:homebrew/homebrew-core";
      flake = false;
    };
    homebrew-cask = {
      url = "github:homebrew/homebrew-cask";
      flake = false;
    };
    homebrew-hashicorp = {
      url = "github:hashicorp/homebrew-tap";
      flake = false;
    };
    home-manager = {
      url = "github:lafrenierejm/home-manager/release-24.11_ripgrep-all";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mac-app-util.url = "github:hraban/mac-app-util";
    mujmap = {
      url = "github:lafrenierejm/mujmap";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        crane.follows = "crane";
        rust-overlay.follows = "rust-overlay";
        pre-commit-hooks.follows = "git-hooks";
      };
    };
    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    ripgrep-all = {
      url = "github:lafrenierejm/ripgrep-all";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        crane.follows = "crane";
        rust-overlay.follows = "rust-overlay";
        pre-commit-hooks.follows = "git-hooks";
      };
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
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
        inputs.git-hooks.flakeModule
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
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            inputs.nur.overlays.default
          ];
        };

        packages = {
          default = pkgs.hello;
          install-iso = let
            username = "lafrenierejm";
            personal = true;
          in
            inputs.nixos-generators.nixosGenerate {
              inherit system;
              format = "install-iso";
              modules = [
                inputs.catppuccin.nixosModules.catppuccin
                ./nix/common.nix
                ./nix/earthbound/configuration.nix
                inputs.disko.nixosModules.disko
                inputs.home-manager.nixosModules.home-manager
                {
                  nixpkgs.overlays = [
                    inputs.emacs-overlay.overlays.default
                  ];
                  home-manager.extraSpecialArgs = {
                    inherit inputs personal system username;
                    gitEmail = "git@lafreniere.xyz";
                    gitUseGpg = true;
                  };
                  home-manager.useGlobalPkgs = true;
                  home-manager.useUserPackages = true;
                  home-manager.modules = [
                    ./nix/home/sway.nix
                  ];
                  home-manager.users."${username}" = import ./nix/home.nix;
                  users.users."${username}" = {
                    home = "/home/${username}";
                    openssh.authorizedKeys.keys = [
                      (builtins.readFile ./ssh/macbook.pub)
                    ];
                  };
                }
              ];
              specialArgs = {inherit inputs personal system username;};
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
            prettier.enable = true;
            shellcheck.enable = true;
            shfmt = {
              enable = true;
              indent_size = null;
            };
            taplo.enable = true;
          };
          settings.formatter = {
            prettier.excludes = ["karabiner/karabiner.json"];
          };
        };

        pre-commit = {
          check.enable = true;
          settings.hooks = {
            editorconfig-checker = {
              enable = true;
              excludes = [
                ".*\\.age"
                ".*\\.gpg"
              ];
            };
            ripsecrets = {
              enable = true;
              excludes = [".*\\.crypt"];
            };
            taplo.enable = true;
            treefmt.enable = true;
            typos = {
              enable = true;
              excludes = [".*\\.crypt"];
            };
          };
        };

        devShells.default = pkgs.mkShell {
          # Inherit all of the pre-commit hooks.
          inputsFrom = [config.pre-commit.devShell config.treefmt.build.devShell];
          packages = config.pre-commit.settings.enabledPackages;
        };
      };

      flake = let
        realName = "Joseph LaFreniere";
        legacyPackages = inputs.nixpkgs.legacyPackages;
        legacyPackagesUnstable = inputs.nixpkgs-unstable.legacyPackages;
      in {
        # The usual flake attributes can be defined here, including system-
        # agnostic ones like nixosModule and system-enumerating ones, although
        # those are more easily expressed in perSystem.
        darwinConfigurations = {
          airborn = let
            personal = true;
            hostname = "airborn";
            userName = "lafrenierejm";
            system = "aarch64-darwin";
            pkgs = import inputs.nixpkgs {
              inherit system;
              overlays = [
                inputs.nur.overlays.default
              ];
            };
            pkgsUnstable = legacyPackagesUnstable."${system}";
            lib = pkgs.lib;
          in
            inputs.darwin.lib.darwinSystem rec {
              inherit system;
              modules = [
                inputs.agenix.nixosModules.default
                inputs.homebrew.darwinModules.nix-homebrew
                inputs.home-manager.darwinModules.home-manager
                inputs.mac-app-util.darwinModules.default
                ./nix/common.nix
                ./nix/darwin.nix
                {
                  nixpkgs.overlays = [
                    inputs.emacs-overlay.overlays.default
                    inputs.nur.overlays.default
                  ];
                  home-manager.sharedModules = [
                    inputs.mac-app-util.homeManagerModules.default
                  ];
                  home-manager.useGlobalPkgs = true;
                  home-manager.useUserPackages = true;
                  home-manager.users."${userName}" = import ./nix/home.nix rec {
                    inherit inputs lib personal pkgs pkgsUnstable userName realName system;
                    gitEmail = "git@lafreniere.xyz";
                    gitUseGpg = true;
                  };
                  users.users."${userName}".home = "/Users/${userName}";
                }
              ];
              specialArgs = {
                inherit inputs system personal userName hostname realName;
              };
            };

          JLAFRENI0523-MB = let
            domain = "renaissance.com";
            hostname = "JLAFRENI0523-MB";
            personal = false;
            userName = "joseph.lafreniere";
            system = "aarch64-darwin";
            pkgs = legacyPackages."${system}";
            pkgsUnstable = legacyPackagesUnstable."${system}";
            lib = pkgs.lib;
          in
            inputs.darwin.lib.darwinSystem rec {
              inherit system;
              modules = [
                inputs.agenix.nixosModules.default
                inputs.homebrew.darwinModules.nix-homebrew
                inputs.home-manager.darwinModules.home-manager
                ./nix/common.nix
                ./nix/darwin.nix
                {
                  nixpkgs.overlays = [
                    inputs.emacs-overlay.overlays.default
                  ];
                  home-manager.useGlobalPkgs = true;
                  home-manager.useUserPackages = true;
                  home-manager.users."${userName}" = import ./nix/home.nix {
                    inherit inputs lib personal pkgs pkgsUnstable userName realName system;
                    gitEmail = "joseph.lafreniere@${domain}";
                    gitUseGpg = true;
                  };
                  users.users."${userName}".home = "/Users/${userName}";
                }
              ];
              specialArgs = {
                inherit inputs personal system hostname realName userName;
              };
            };
        };
        nixosConfigurations = {
          earthbound = let
            userName = "lafrenierejm";
            domain = "lafreniere.xyz";
            system = "x86_64-linux";
            personal = true;
            pkgs = inputs.nixpkgs.legacyPackages."${system}";
            pkgsUnstable = legacyPackagesUnstable."${system}";
            lib = pkgs.lib;
          in
            inputs.nixpkgs.lib.nixosSystem {
              inherit system;
              modules = [
                inputs.agenix.nixosModules.default
                inputs.catppuccin.nixosModules.catppuccin
                inputs.disko.nixosModules.disko
                inputs.home-manager.nixosModules.home-manager
                ./nix/common.nix
                ./nix/earthbound/configuration.nix
                {
                  nixpkgs.overlays = [
                    inputs.emacs-overlay.overlays.default
                  ];
                  home-manager.backupFileExtension = "bak";
                  home-manager.useGlobalPkgs = true;
                  home-manager.useUserPackages = true;
                  home-manager.users."${userName}" =
                    import ./nix/home.nix {
                      inherit inputs personal system realName userName pkgs pkgsUnstable lib;
                      gitEmail = "git@${domain}";
                      gitUseGpg = true;
                    }
                    // {
                      imports = [
                        inputs.catppuccin.homeManagerModules.catppuccin
                        ./nix/home/theme.nix
                        ./nix/home/sway.nix
                        ./nix/home/udiskie.nix
                      ];
                    };
                  users.users = {
                    "${userName}" = {
                      home = "/home/${userName}";
                      isNormalUser = true;
                      description = "Joseph LaFreniere";
                      openssh.authorizedKeys.keys = [
                        (builtins.readFile ./ssh/macbook.pub)
                        (builtins.readFile ./ssh/JLAFRENI0523-MB.renaissance.com.pub)
                      ];
                      extraGroups = [
                        "inputs"
                        "networkmanager"
                        "media"
                        "podman"
                        "wheel"
                      ];
                      shell = pkgs.zsh;
                    };
                    rbralley = {
                      isNormalUser = true;
                      description = "Ratannya Bralley";
                    };
                  };
                }
              ];
              specialArgs = {inherit inputs domain personal realName system userName;};
            };
        };
      };
    };
}
