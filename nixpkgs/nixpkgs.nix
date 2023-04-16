# A nixpkgs instance that is grabbed from the pinned nixpkgs commit in the lock file.
# This is useful to avoid using channels when using legacy nix commands.
# https://github.com/Misterio77/nix-starter-configs/blob/691c6579c78d770a7cb67b48119fa3fedef9e363/standard/nixpkgs.nix

let
  lock =
    (builtins.fromJSON (builtins.readFile ./flake.lock)).nodes.nixpkgs.locked;
in import (fetchTarball {
  url = "https://github.com/nixos/nixpkgs/archive/${lock.rev}.tar.gz";
  sha256 = lock.narHash;
})
