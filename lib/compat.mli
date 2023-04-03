type doc := Doc.t

type _ formatter
type rdoc
type doc_fmt = rdoc formatter


type ('a,'impl) printer = 'impl formatter -> 'a -> unit
type 'a final_printer = ('a, Format.formatter) printer
type 'a generic_printer = { printer: 'impl. ('a,'impl) printer }

val make_formatter: Format.formatter -> Format.formatter formatter
val formatter_of_out_channel: out_channel -> Format.formatter formatter
val make_doc: doc ref -> doc_fmt
val compat: ('a,Format.formatter) printer -> Format.formatter -> 'a -> unit

val doc: doc_fmt -> doc
val formatter: Format.formatter formatter -> Format.formatter


val fprintf : 'impl formatter -> ('a,'impl formatter,unit) format -> 'a
val kfprintf:
  ('impl formatter -> 'a) -> 'impl formatter ->
  ('b, 'impl formatter, unit, 'a) format4 -> 'b


val asprintf :  ('a, Format.formatter formatter, unit, string) format4 -> 'a
val kasprintf : (string -> 'a) ->
  ('b, Format.formatter formatter, unit, 'a) format4 -> 'b


val dprintf : ('a,'impl formatter, unit, 'impl formatter -> unit) format4 -> 'a
val kdprintf:
  (('impl formatter -> unit) -> 'a) ->
  ('b, 'impl formatter, unit, 'a) format4 -> 'b

val doc_printf: ('a, rdoc formatter, unit, doc) format4 -> 'a



val format_printer: 'a final_printer -> Format.formatter -> 'a -> unit
val doc_printer:('a, rdoc) printer -> 'a -> Immutable.printer


val pp_doc: (doc,_) printer

val pp_print_string: (string,_) printer
val pp_print_text: (string,_) printer
val pp_print_char: (char,_) printer
val pp_print_int: (int,_) printer
val pp_print_float: (float,_) printer


val pp_print_list:
  ?pp_sep:(unit,'impl) printer -> ('a,'impl) printer -> ('a list, 'impl) printer


val pp_print_option:
  ?none:(unit,'impl) printer -> ('a,'impl) printer -> ('a option, 'impl) printer
val pp_open_stag: (Format.stag,_) printer
val pp_close_stag: (unit,_) printer

val pp_open_box: (int,_) printer
val pp_close_box: (unit,_) printer

val pp_print_space: (unit,_) printer
val pp_print_cut: (unit,_) printer
val pp_print_break: _ formatter -> int -> int -> unit


val pp_open_tbox: (unit,_) printer
val pp_close_tbox: (unit,_) printer
val pp_set_tab: (unit,_) printer
val pp_print_tab: (unit,_) printer
val pp_print_tbreak: 'impl formatter -> int -> int -> unit

val pp_print_newline: (unit,_) printer
val pp_force_newline: (unit,_) printer


(** {1 Compiler output} *)


val pp_two_columns :
  ?sep:string -> ?max_lines:int ->
  'impl formatter -> (string * string) list -> unit
(** [pp_two_columns ?sep ?max_lines ppf l] prints the lines in [l] as two
   columns separated by [sep] ("|" by default). [max_lines] can be used to
   indicate a maximum number of lines to print -- an ellipsis gets inserted at
   the middle if the input has too many lines.

   Example:

    {v pp_two_columns ~max_lines:3 Format.std_formatter [
      "abc", "hello";
      "def", "zzz";
      "a"  , "bllbl";
      "bb" , "dddddd";
    ] v}

    prints

    {v
    abc | hello
    ...
    bb  | dddddd
    v}
*)
