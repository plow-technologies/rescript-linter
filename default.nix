with import <nixpkgs> {};
let
  ocaml = ocamlPackages.ocaml;
  opam2nix = import ./nix/opam2nix.nix {};
  selection = opam2nix.build {
    inherit ocaml;
    selection = ./nix/opam-selection.nix;
    src = ./.;
  };
in
selection.rescript_linter
