(lang dune 3.17)

(name rgxx)

(generate_opam_files true)

(source
 (github "44mira/rgxx"))

(authors "Legolas Tyrael Lada")

(maintainers "Legolas Tyrael Lada")

(license LICENSE)

(documentation https://github.com/44mira/rgxx)

(package
 (name rgxx)
 (synopsis "A regex validator.")
 (description "A regex validator in OCaml.")
 (depends ocaml sexplib))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
