{}:
let
  pkgs = (import <nixpkgs> {});
in (pkgs.stdenv.mkDerivation rec {
  dune = pkgs.callPackage ./dune.nix {
    lib = pkgs.lib;
    findlib = pkgs.ocamlPackages.findlib;
  };

  name = "rescript_linter";
  pname = name;

  src = pkgs.lib.cleanSourceWith {
    name = "rescript_linter";
    src = ./.;
  };

  buildInputs = with pkgs; [ ocaml dune ];

  buildPhase = ''
    dune build -p rescript_linter
  '';

  installPhase = ''
    cp _build/install/default/bin/rescript_linter $out
  '';
})
