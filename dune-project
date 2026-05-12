(lang dune 3.22)

(name liszoo)

(generate_opam_files true)

(source
 (github desumn/liszoo))

(authors "desumn <desumn@posteo.net>")

(maintainers "desumn <desumn@posteo.net>")

(license BSD-3)

(package
 (name liszoo)
 (synopsis "A zoo of static analysis on a small language.")
 (description
  "A zoo of static analysis on a small language, mostly as a personal learning project.")
 (depends
  ocaml
  (alcotest :with-test)
  menhir
  menhirLib)
 (tags ("static analysis")))
