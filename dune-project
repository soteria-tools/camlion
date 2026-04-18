(lang dune 3.21)

(name camlion)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 2.1)

(package
 (name camlion)
 (synopsis "A parser for the Amazon Ion data format")
 (description
  "OCaml library for parsing the Ion data language (text format)")
  (depends
   ocaml
   (menhir
    (>= 20230608))
   (zarith
    (>= 1.12))
   (parseff
    (>= 0.1))
   (alcotest :with-test))
 (tags
  ("ion" "parser" "data-format")))
