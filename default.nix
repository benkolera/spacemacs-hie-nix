{ nixpkgs ? import <nixpkgs> {} }:
let
  /* You really want to add hie-nix to your nix binary caches or you will have
  to wait a long while. See https://hie-nix.cachix.org/ for the public key and
  cache uri. Those steps work if you aren't on nixos and are just using nix on
  a non-nixos machine.

  If you are on nixos just add the following to your configuration.nix:
  Add "https://hie-nix.cachix.org" to nix.binaryCaches
  and "hie-nix.cachix.org-1:EjBSHzF6VmDnzqlldGXbi0RM3HdjfTU3yDRi9Pd0jTY=" to nix.binaryCachePublicKeys
  */
  hie-src = import nix/hie-nix.nix {};
  hies    = import hie-src {};
  hpkgs   = nixpkgs.pkgs.haskellPackages;
in {
  hie-spacemacs-bundle = {
    hie84 = hies.hie84;
    inherit (hpkgs) apply-refact hasktags hlint hoogle hsimport brittany;
  };
}
