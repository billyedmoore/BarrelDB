(lang dune 3.17)

(name barrelDB)

(generate_opam_files true)

(source
 (github billyedmoore/BarrelDB))

(authors "Billy Moore <author@example.com>")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name barrelDB)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml (ounit2 :with-test))
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
