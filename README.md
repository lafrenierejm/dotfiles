# Joseph LaFreniere (lafrenierejm)'s Dotfiles

## Applying

### macOS (Darwin)

1. `nix build -v ".#darwinConfigurations.$(hostname).system"`
1. `./result/sw/bin/darwin-rebuild switch --flake .`
