# Joseph LaFreniere's Dotfiles

## Applying

### macOS (Darwin)

1. `nix build .#darwinConfigurations.macbook.system`
1. `./result/sw/bin/darwin-rebuild switch --flake .`
