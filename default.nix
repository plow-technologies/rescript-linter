{ pkgs ? import <nixpkgs> { overlays = [ (import ./nix/overlays/gup-0_9_0.nix)]; } }:
let
  ocaml = pkgs.ocaml-ng.ocamlPackages_4_08.ocaml;
  # Use this version of ocaml in pkgs
  myPkgs = pkgs.ocaml-ng.ocamlPackages_4_08 // pkgs;
  opam2nix = import ./nix/opam2nix.nix {pkgs=myPkgs;};
  selection = opam2nix.build {
    inherit ocaml;
    selection = ./nix/opam-selection.nix;
    src = ./.;
  };
in
selection.rescript_linter
