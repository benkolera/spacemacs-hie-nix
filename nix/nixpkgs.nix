import (builtins.fetchGit {
  # Descriptive name to make the store path easier to identify
  name = "nixos-unstable-2018-11-18";
  url = https://github.com/nixos/nixpkgs/;
  # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
  rev = "80738ed9dc0ce48d7796baed5364eef8072c794d";
}) {}
