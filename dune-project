(lang dune 3.3)

(name rescript_linter)

(version 0.0.2)

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
 (depends ocaml dune))
