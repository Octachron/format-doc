(lang dune 3.2)

(generate_opam_files true)

(source (github Octachron/format-doc))
(license LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception)
(authors "Florian Angeletti")
(maintainers "octa@polychoron.fr")

(package
 (name format-doc)
 (version 0.1)
 (synopsis "Alternative interpreters for OCaml format strings")
 (description "Format-doc provides alternative interpreters for OCaml format strings:
    - Format_doc.Immutable provides an immutable interpreter that records formatting
     instruction that can be send subsequently to a Format.formatter.
    - Format_doc.Ref is a thin layer above Format_doc.Immutable that wraps the
      document in a reference cell in order to mimic the classical Format API.
    - Format_doc.Compat provides a compatibility interpreter that may work in either
      the classical Format mode or in the reference mode from above.")
)
