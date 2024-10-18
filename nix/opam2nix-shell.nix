# Make a nix shell for using opam2nix
{ pkgs ? import <nixpkgs> {} }:
let
    older_nix_pkgs = import (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/e89cf1c932006531f454de7d652163a9a5c86668.tar.gz";
    }) {};

    gup-0_9_0 = pkgs.gup;

    # Replaces gup in nixpkgs with the older version
    pkgs = older_nix_pkgs // {
        gup = gup-0_9_0;
    };

    opam2nix = import ./opam2nix.nix {pkgs=pkgs;};
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    opam2nix
  ];
}