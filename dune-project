(lang dune 3.0)
(name digit-dp-lib)
(authors "samosica <samosic4@gmail.com>")
(maintainers "samosica <samosic4@gmail.com>")
(source (github samosica/digit-dp-lib))
(generate_opam_files true)

(package
  (name digit-dp-lib)
  (synopsis "A library of digit DP and automaton DP")
  (depends
    (ocaml (>= 4.10))
    dune
    (ppx_inline_test :with-test)
    (ppx_expect :with-test)))