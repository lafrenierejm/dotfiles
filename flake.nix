{
  description = "Joseph LaFreniere (lafrenierejm)'s dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-root.url = "github:srid/flake-root";
    pre-commit-hooks-nix = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.flake-root.flakeModule
        inputs.pre-commit-hooks-nix.flakeModule
        inputs.treefmt-nix.flakeModule
      ];
      systems = [ "x86_64-linux" "aarch64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }: {
        # Per-system attributes can be defined here. The self' and inputs'
        # module parameters provide easy access to attributes of the same
        # system.

        # Equivalent to  inputs'.nixpkgs.legacyPackages.hello;
        packages.default = pkgs.hello;

        # Auto formatters. This also adds a flake check to ensure that the
        # source tree was auto formatted.
        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;
          flakeCheck = false; # use pre-commit's check instead

          programs.nixpkgs-fmt.enable = true;
          programs.shfmt.enable = true;
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
          inputsFrom =
            [ config.flake-root.devShell config.pre-commit.devShell ];
        };
      };
      # flake = {
      #   # The usual flake attributes can be defined here, including system-
      #   # agnostic ones like nixosModule and system-enumerating ones, although
      #   # those are more easily expressed in perSystem.
      # };
    };
}
