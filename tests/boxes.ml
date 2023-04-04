open Format_doc
open Immutable

let doc =
  printf "@[<v 2>First box@ @[<%s>Second@ box@]@]"
    "v 4"
    Doc.empty

let () = Doc.format Format.std_formatter doc
