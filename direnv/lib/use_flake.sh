# Copied verbatim from https://nixos.wiki/wiki/Flakes#Direnv_integration on 2022-01-01.
# Per https://nixos.wiki/wiki/NixOS_Wiki:Copyrights:
# Copyright (c) 2017 Jörg Thalheim
# Licensed under the MIT license.

use_flake() {
  watch_file flake.nix
  watch_file flake.lock
  eval "$(nix print-dev-env --profile "$(direnv_layout_dir)/flake-profile")"
}
