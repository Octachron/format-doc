type doc := Doc.t

type ('a,'b) fmt = ('a, doc, doc,'b) format4

type printer = doc -> doc

val printf: ('a, printer) fmt -> 'a
val msg: ('a,doc) fmt -> 'a
val kmsg: (doc -> 'b) -> ('a,'b) fmt -> 'a
val kprintf: (doc -> 'b) -> ('a, doc -> 'b) fmt -> 'a

val open_box: Doc.box_type -> int -> printer
val close_box: printer

val text: string -> printer
val string: string -> printer
val bytes: bytes -> printer
val with_size: int -> printer

val int: int -> printer
val float: float -> printer
val char: char -> printer
val bool: bool -> printer

val space: printer
val cut: printer
val break: spaces:int -> indent:int -> printer

val custom_break:
  fits:(string * int * string as 'a) -> breaks:'a -> printer
val force_newline: printer
val if_newline: printer

val flush: printer
val force_stop: printer

val open_tbox: printer
val set_tab: printer
val tab: printer
val tab_break: width:int -> offset:int -> printer
val close_tbox: printer

val open_tag: Doc.tag -> printer
val close_tag: printer

val list:
  ?sep:(doc->doc) -> ('a -> printer) -> 'a list -> printer

val option: ?none:(doc->doc) -> ('a -> doc -> doc) -> 'a option -> doc -> doc


val output_formatting_lit: CamlinternalFormatBasics.formatting_lit -> doc -> doc
val to_string: doc -> string
