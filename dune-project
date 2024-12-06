(lang dune 3.17)

(name aoc2024)

(generate_opam_files true)

(source
 (github itsfrank/adventofcode-2024))

(authors "Francis O'Brien <github.com/itsfrank>")

(maintainers "Francis O'Brien <github.com/itsfrank>")

(license MIT)

(package
 (name aoc2024)
 (synopsis "frank's advent of code 2024")
 (description "frank's advent of code 2024")
 (depends
  ocaml
  containers
  ppx_string
  ;; test
  (ppx_inline_test :with-test)
  (ppx_inline_alcotest :with-test)
  (alcotest :with-test)
  ;; dev tools
  (ocamlformat :dev)
  (ocaml-lsp-server :dev)))
