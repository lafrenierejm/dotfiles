{
  description = "Joseph LaFreniere (lafrenierejm)'s dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-24.11";
    nixpkgs-trunk.url = "github:NixOS/nixpkgs";
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
        legacyPackagesTrunk = inputs.nixpkgs-trunk.legacyPackages;
      in {
        # The usual flake attributes can be defined here, including system-
        # agnostic ones like nixosModule and system-enumerating ones, although
        # those are more easily expressed in perSystem.
        darwinConfigurations = let
          pkgs = legacyPackages."${system}";
          pkgsTrunk = legacyPackagesTrunk."${system}";
          lib = pkgs.lib;
          system = "aarch64-darwin";
          hosts = {
            airborn = {
              personal = true;
              hostname = "airborn";
              userName = "lafrenierejm";
              gitEmail = "git@lafreniere.xyz";
            };
            JLAFRENI0523-MB = rec {
              domain = "renaissance.com";
              hostname = "JLAFRENI0523-MB";
              personal = false;
              userName = "joseph.lafreniere";
              gitEmail = "${userName}@${domain}";
            };
          };
        in (builtins.mapAttrs
          (host: values: (inputs.darwin.lib.darwinSystem {
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
                ];
                home-manager.sharedModules = [
                  inputs.mac-app-util.homeManagerModules.default
                ];
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users."${values.userName}" = import ./nix/home.nix rec {
                  inherit inputs lib pkgs pkgsTrunk realName system;
                  inherit (values) gitEmail personal userName;
                  gitUseGpg = true;
                };
                users.users."${values.userName}".home = "/Users/${values.userName}";
              }
            ];
            specialArgs = {
              inherit inputs system pkgs pkgsTrunk realName;
              inherit (values) personal userName hostname;
            };
          }))
          hosts);

        nixosConfigurations = {
          earthbound = let
            userName = "lafrenierejm";
            domain = "lafreniere.xyz";
            system = "x86_64-linux";
            personal = true;
            pkgs = inputs.nixpkgs.legacyPackages."${system}";
            pkgsTrunk = legacyPackagesTrunk."${system}";
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
                      inherit inputs personal system realName userName pkgs pkgsTrunk lib;
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
