{ nixpkgs ? import ./nixpkgs.nix }:
nixpkgs.pkgs.fetchgit {
  inherit (nixpkgs.pkgs.lib.importJSON ./hie-nix.json) url rev sha256;
}
