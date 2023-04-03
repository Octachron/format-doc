(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Doc

(** Compatibility interface *)

type rdoc = doc ref
type _ formatter =
  | Format: Format.formatter -> Format.formatter formatter
  | Doc: rdoc -> rdoc formatter
type doc_fmt = rdoc formatter


type ('a,'impl) printer = 'impl formatter -> 'a -> unit

type 'a final_printer = ('a, Format.formatter) printer
type 'a generic_printer = { printer: 'impl. ('a,'impl) printer}

let make_formatter fmt  = Format fmt
let formatter_of_out_channel chan =
  make_formatter (Format.formatter_of_out_channel chan)
let make_doc d = Doc d

let doc: rdoc formatter -> _ = function
  | Doc { contents = rd } -> rd
  | Format  _ ->
      (* Format.formatter is not a ref *)
      assert false
let formatter: Format.formatter formatter -> _ = function
  | Doc _ ->
      (* Format.formatter is not a ref *)
      assert false
  | Format ppf -> ppf


(** {1 Primitive functions }*)


let pp_print_string (type i) (ppf: i formatter) s = match ppf with
  | Format ppf -> Format.pp_print_string ppf s
  | Doc rdoc -> rdoc := Immutable.string s !rdoc

let pp_print_text (type i) (ppf: i formatter) s = match ppf with
  | Format ppf -> Format.pp_print_text ppf s
  | Doc rdoc -> rdoc := Immutable.text s !rdoc

let pp_print_char (type i) (ppf: i formatter) c = match ppf with
  | Format ppf -> Format.pp_print_char ppf c
  | Doc rdoc -> rdoc := Immutable.char c !rdoc

let pp_print_int (type i) (ppf: i formatter) c = match ppf with
  | Format ppf -> Format.pp_print_int ppf c
  | Doc rdoc -> rdoc := Immutable.int c !rdoc

let pp_print_float (type i) (ppf: i formatter) c = match ppf with
  | Format ppf -> Format.pp_print_float ppf c
  | Doc rdoc -> rdoc := Immutable.float c !rdoc

let pp_close_box (type i) (ppf: i formatter) () = match ppf with
  | Format ppf -> Format.pp_close_box ppf ()
  | Doc rdoc -> rdoc := Immutable.close_box !rdoc

let pp_close_stag (type i) (ppf: i formatter) () = match ppf with
  | Format ppf -> Format.pp_close_stag ppf ()
  | Doc rdoc -> rdoc := Immutable.close_tag !rdoc

let pp_print_break (type i) (ppf: i formatter) spaces indent = match ppf with
  | Format ppf -> Format.pp_print_break ppf spaces indent
  | Doc rdoc -> rdoc := Immutable.break ~spaces ~indent !rdoc

let pp_print_space ppf () = pp_print_break ppf 1 0
let pp_print_cut ppf () = pp_print_break ppf 0 0


let pp_print_flush (type i) (ppf: i formatter) () = match ppf with
  | Format ppf -> Format.pp_print_flush ppf ()
  | Doc rdoc -> rdoc := Immutable.flush !rdoc

let pp_force_newline (type i) (ppf: i formatter) () = match ppf with
  | Format ppf -> Format.pp_force_newline ppf ()
  | Doc rdoc -> rdoc := Immutable.force_newline !rdoc

let pp_print_newline (type i) (ppf: i formatter) () = match ppf with
  | Format ppf -> Format.pp_print_newline ppf ()
  | Doc rdoc -> rdoc := Immutable.force_stop !rdoc


let pp_print_as (type i) (ppf: i formatter) size x = match ppf with
  | Format ppf -> Format.pp_print_as ppf size x
  | Doc rdoc ->
      rdoc := !rdoc |> Immutable.with_size size |> Immutable.string x

let pp_open_stag (type i) (ppf: i formatter) stag = match ppf with
  | Format ppf -> Format.pp_open_stag ppf stag
  | Doc rdoc -> rdoc := !rdoc |> Immutable.open_tag stag

let pp_open_box_gen (type i) (ppf: i formatter) indent bxty =
  let box_type = Doc.box_type bxty in
  match ppf with
  | Format ppf -> format_open_box_gen ppf box_type indent
  | Doc rdoc -> rdoc := !rdoc |> Immutable.open_box box_type indent

let pp_open_box ppf indent = pp_open_box_gen ppf indent Pp_box


let pp_open_tbox (type i) (ppf: i formatter) () =
  match ppf with
  | Format ppf -> Format.pp_open_tbox ppf ()
  | Doc rdoc -> rdoc := !rdoc |> Immutable.open_tbox

let pp_close_tbox (type i) (ppf: i formatter) () =
  match ppf with
  | Format ppf -> Format.pp_close_tbox ppf ()
  | Doc rdoc -> rdoc := !rdoc |> Immutable.close_tbox

let pp_set_tab (type i) (ppf: i formatter) () =
  match ppf with
  | Format ppf -> Format.pp_set_tab ppf ()
  | Doc rdoc -> rdoc := !rdoc |> Immutable.set_tab

let pp_print_tab (type i) (ppf: i formatter) () =
  match ppf with
  | Format ppf -> Format.pp_print_tab ppf ()
  | Doc rdoc -> rdoc := !rdoc |> Immutable.tab

let pp_print_tbreak (type i) (ppf: i formatter) width offset =
  match ppf with
  | Format ppf -> Format.pp_print_tbreak ppf width offset
  | Doc rdoc -> rdoc := !rdoc |> Immutable.tab_break ~width ~offset


let pp_doc (type i) (ppf: i formatter) doc = match ppf with
  | Format ppf -> format ppf doc
  | Doc rdoc -> rdoc := Doc.append !rdoc doc

module Driver = struct
  (* Interpret a formatting entity on a formatter. *)
  let output_formatting_lit ppf
      (fmting_lit:CamlinternalFormatBasics.formatting_lit)
    = match fmting_lit with
    | Close_box                 -> pp_close_box ppf ()
    | Close_tag                 -> pp_close_stag ppf ()
    | Break (_, width, offset)  -> pp_print_break ppf width offset
    | FFlush                    -> pp_print_flush ppf ()
    | Force_newline             -> pp_force_newline ppf ()
    | Flush_newline             -> pp_print_newline ppf ()
    | Magic_size (_, _)         -> ()
    | Escaped_at                -> pp_print_char ppf '@'
    | Escaped_percent           -> pp_print_char ppf '%'
    | Scan_indic c              -> pp_print_char ppf '@'; pp_print_char ppf c



  let compute_tag (type i) (main: i formatter) output tag_acc =
    let buf = Buffer.create 16 in
    let buf_fmt = Format.formatter_of_buffer buf in
    let ppf, commit = match main with
      | Format _ -> (Format buf_fmt: i formatter) , ignore
      | Doc _ ->
          let rdoc = ref empty in
          (Doc rdoc: i formatter),
          fun () -> format buf_fmt !rdoc; Format.pp_print_flush buf_fmt ()
    in
    output ppf tag_acc;
    pp_print_flush ppf ();
    commit ();
    let len = Buffer.length buf in
    if len < 2 then Buffer.contents buf
    else Buffer.sub buf 1 (len - 2)

  (* Recursively output an "accumulator" containing a reversed list of
     printing entities (string, char, flus, ...) in an output_stream. *)
  (* Differ from Printf.output_acc by the interpretation of formatting. *)
  (* Used as a continuation of CamlinternalFormat.make_printf. *)
  let rec output_acc: type i.
    i formatter -> (i formatter, unit) CamlinternalFormat.acc -> unit =
    fun ppf acc ->
    match acc with
    | Acc_string_literal (Acc_formatting_lit (p, Magic_size (_, size)), s)
    | Acc_data_string (Acc_formatting_lit (p, Magic_size (_, size)), s) ->
        output_acc ppf p;
        pp_print_as ppf size s;
    | Acc_char_literal (Acc_formatting_lit (p, Magic_size (_, size)), c)
    | Acc_data_char (Acc_formatting_lit (p, Magic_size (_, size)), c) ->
        output_acc ppf p;
        pp_print_as ppf size (String.make 1 c);
    | Acc_formatting_lit (p, f) ->
        output_acc ppf p;
        output_formatting_lit ppf f;
    | Acc_formatting_gen (p, Acc_open_tag acc') ->
        output_acc ppf p;
        pp_open_stag ppf (Format.String_tag (compute_tag ppf output_acc acc'))
    | Acc_formatting_gen (p, Acc_open_box acc') ->
        output_acc ppf p;
        let (indent, bty) =
          let box_info = compute_tag ppf output_acc acc' in
          CamlinternalFormat.open_box_of_string box_info
        in
        pp_open_box_gen ppf indent bty
    | Acc_string_literal (p, s)
    | Acc_data_string (p, s)   -> output_acc ppf p; pp_print_string ppf s;
    | Acc_char_literal (p, c)
    | Acc_data_char (p, c)     -> output_acc ppf p; pp_print_char ppf c;
    | Acc_delay (p, f)         -> output_acc ppf p; f ppf;
    | Acc_flush p              -> output_acc ppf p; pp_print_flush ppf ();
    | Acc_invalid_arg (p, msg) -> output_acc ppf p; invalid_arg msg;
    | End_of_acc               -> ()
end

let kfprintf k ppf (CamlinternalFormatBasics.Format (fmt, _))  =
  CamlinternalFormat.make_printf
    (fun acc -> Driver.output_acc ppf acc; k ppf)
    End_of_acc fmt
let fprintf doc fmt = kfprintf ignore doc fmt

let kasprintf k fmt =
  let b = Buffer.create 20 in
  let ppf = make_formatter (Format.formatter_of_buffer b) in
  kfprintf
    (fun ppf -> pp_print_flush ppf ();
      let r = Buffer.contents b in
      Buffer.reset b;
      k r)
    ppf fmt

let asprintf fmt = kasprintf Fun.id fmt

let kdprintf k (CamlinternalFormatBasics.Format (fmt, _)) =
  CamlinternalFormat.make_printf
    (fun acc -> k (fun ppf -> Driver.output_acc ppf acc))
    End_of_acc fmt

let dprintf fmt = kdprintf (fun i -> i) fmt


let doc_printf fmt = kfprintf (fun ppf -> doc ppf) (make_doc (ref empty)) fmt


let doc_printer f x doc =
  let r = ref doc in
  f (Doc r) x;
  !r
let format_printer f ppf x = f (make_formatter ppf) x

let pp_print_list (type i) ?(pp_sep:(unit,i) printer=pp_print_cut)
    (elt: ('a,i) printer) (ppf: i formatter) l  = match ppf with
  | Format ppf ->
      let pp_sep ppf x = pp_sep (Format ppf) x in
      let elt ppf x = elt(Format ppf) x in
      Format.pp_print_list ~pp_sep elt ppf l
  | Doc rdoc ->
      rdoc :=
        Immutable.list ~sep:(doc_printer pp_sep ()) (doc_printer elt) l !rdoc


let pp_print_option (type i) ?(none:(unit,i) printer=fun _ () -> ())
    (elt: ('a,i) printer) (ppf: i formatter) o  = match ppf with
  | Format ppf ->
      let none ppf x = none (Format ppf) x in
      let elt ppf x = elt(Format ppf) x in
      Format.pp_print_option ~none elt ppf o
  | Doc rdoc ->
      rdoc :=
        Immutable.option ~none:(doc_printer none ()) (doc_printer elt) o !rdoc



let pp_two_columns ?(sep = "|") ?max_lines ppf (lines: (string * string) list) =
  let left_column_size =
    List.fold_left (fun acc (s, _) -> Int.max acc (String.length s)) 0 lines in
  let lines_nb = List.length lines in
  let ellipsed_first, ellipsed_last =
    match max_lines with
    | Some max_lines when lines_nb > max_lines ->
        let printed_lines = max_lines - 1 in (* the ellipsis uses one line *)
        let lines_before = printed_lines / 2 + printed_lines mod 2 in
        let lines_after = printed_lines / 2 in
        (lines_before, lines_nb - lines_after - 1)
    | _ -> (-1, -1)
  in
  fprintf ppf "@[<v>";
  List.iteri (fun k (line_l, line_r) ->
      if k = ellipsed_first then fprintf ppf "...@,";
      if ellipsed_first <= k && k <= ellipsed_last then ()
      else fprintf ppf "%*s %s %s@," left_column_size line_l sep line_r
    ) lines;
  fprintf ppf "@]"

let compat = format_printer
