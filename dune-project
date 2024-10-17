(lang dune 3.16)

(name rescript_linter)

(version 0.3.0)

(generate_opam_files true)

(source
 (github plow-technologies/rescript-linter))

(authors "Shulhi Sapli")

(maintainers "Shulhi Sapli")

(license LICENSE)

(package
 (name rescript_linter)
 (synopsis "AST-based linter for ReScript")
 (description "AST-based linter for ReScript")
 (depends ocaml dune yojson alcotest ounit2))
