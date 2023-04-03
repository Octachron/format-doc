
(** Composable document for the Format formatting engine *)

type box_type =
  | H
  | V
  | HV
  | HoV
  | B
val box_type: CamlinternalFormatBasics.block_type -> box_type
val format_open_box_gen:  Format.formatter -> box_type -> int -> unit

type tag = Format.stag = ..

type element =
  | Data of string
  | With_size of int
  | Open_box of { kind: box_type ; indent:int }
  | Close_box
  | Open_tag of Format.stag
  | Close_tag
  | Open_tbox
  | Tab_break of { width : int; offset : int }
  | Set_tab
  | Close_tbox
  | Simple_break of { spaces : int; indent : int }
  | Break of { fits : string * int * string as 'a; breaks : 'a }
  | Flush of { newline:bool }
  | Newline
  | If_newline


type t
type doc = t

val empty: t

val format: Format.formatter -> doc -> unit
val fold: ('acc -> element -> 'acc) -> 'acc -> doc -> 'acc
val add: element -> doc -> doc
val append: doc -> doc -> doc
