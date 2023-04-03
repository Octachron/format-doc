Format-doc provides alternative interpreters for OCaml format strings:

- `Format_doc.Immutable` provides an immutable interpreter that records formatting
instruction that can be send subsequently to a Format.formatter.

- `Format_doc.Ref` is a thin layer above `Format_doc.Immutable` that wraps the
document in a reference cell in order to mimic the classical Format API.

- `Format_doc.Compat` provides a compatibility interpreter that may work in either
the classical Format mode or in the reference mode from above."

## Immutable interface

The `Immutable` module provides a way to compose partial message
without mutation

``` ocaml
open Format_doc.Immutable
module Doc = Format_doc.Doc

type Doc.tag += Red

let test_doc = 
  Doc.empty
  |> open_box B 0
  |> string "Let's start with a string."
  |> printf "@,%s@ %t" "We can use regular"
    (list string ["format strings "; "and immutable printer"])
  |> open_tag Red
  |> printf "@ and tag."
  |> close_tag
  |> close_box
  |> force_stop
```

The advantage is that the `Doc.t` consists in pure data and it is thus
fully serializable and independent on the interpretation of tags and
formatting hints.

In particular, this means that we can easily print the same document with
different interpretation of tags or geometry without resorting to a closure

``` ocaml
let () =
  Format.set_geometry ~max_indent:19 ~margin:20;
  Doc.format Format.std_formatter test_doc 
```

## Compatibility interface

The `Compat` module has been designed to be able to replace in-situ the `Format`
module with exactly the same API while giving the possibility to use either the
`Format_doc.Immutable` backend or the classical backend.

``` ocaml
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
```

Switching to the classical backend is then just a function call away

``` ocaml
  let () = compat print Format.std_formatter ()
```
but we can still choose the immutable backend with
``` ocaml
let test_doc' = doc_printf "%t" print () 
```
and use the fact that `Doc.t` is pure data to check that we ended up
with the same `Doc.t` with the compatibility layer:

``` ocaml
let _ = assert (test_doc = test_doc')
```
