open Format_doc.Immutable
module Doc = Format_doc.Doc

type Doc.tag += Red

let test_doc =
     Doc.empty
  |> open_box B 0
  |> string "Let's start with a string."
  |> printf "@,%s@ %t" "We can use regular"
    (list ~sep:cut string ["format strings "; "and immutable printer"])
  |> open_tag Red
  |> printf "@ and tags."
  |> close_tag
  |> close_box
  |> force_stop


let () =
  Format.set_geometry ~max_indent:19 ~margin:20;
  Doc.format Format.std_formatter test_doc

open Format_doc.Compat
let print ppf ()  =
  pp_open_box ppf 0;
  pp_print_string ppf "Let's start with a string.";
  fprintf ppf "@,%s@ %a"
  "We can use regular"
  (pp_print_list pp_print_string) ["format strings "; "and immutable printer"];
  pp_open_stag ppf Red;
  fprintf ppf "@ and tags.";
  pp_close_stag ppf ();
  pp_close_box ppf ();
  pp_print_newline ppf ()


let () = compat print Format.std_formatter ()

let test_doc' = doc_printf "%a" print ()

let _ = assert (test_doc = test_doc')

let () =
  Format.printf "%a@." Doc.format test_doc'
