(lang dune 3.6)

(name stratego)

(generate_opam_files true)

(source
 (github jan-pi-sona-lili/strategocaml))

(authors "ilo Kali")

(maintainers "ilo Kali")

(license BSD-3-Clause-Clear)

(documentation about:blank)

(package
 (name stratego)
 (synopsis "Terminal-based Stratego")
 (description "A terminal version of Classic Stratego using Notty")
 (depends ocaml dune notty)
 (tags
  (topics stratego game games)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
