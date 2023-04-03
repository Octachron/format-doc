
type box_type =
  | H
  | V
  | HV
  | HoV
  | B

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
  | Simple_break of { spaces : int; indent: int }
  | Break of { fits : string * int * string as 'a; breaks : 'a }
  | Flush of { newline:bool }
  | Newline
  | If_newline

type doc = {front: element list; back:element list}

type t = doc

let empty : doc = { front = []; back = [] }

let to_list doc = doc.front @ List.rev doc.back
let _prepend x doc = { doc with front = x :: doc.front }
let add x doc = { doc with back = x :: doc.back }

let fold f acc doc =
  let first = List.fold_left f acc doc.front in
  let back = List.fold_left f first (List.rev doc.back) in
  back


let append left right = fold (fun doc elt -> add elt doc) left right


let format_open_box_gen ppf kind indent =
  match kind with
  | H-> Format.pp_open_hbox ppf ()
  | V -> Format.pp_open_vbox ppf indent
  | HV -> Format.pp_open_hvbox ppf indent
  | HoV -> Format.pp_open_hovbox ppf indent
  | B -> Format.pp_open_box ppf indent

let interpret_elt ppf = function
  | Data x -> Format.pp_print_string ppf x
  | Open_box { kind; indent } -> format_open_box_gen ppf kind indent
  | Close_box -> Format.pp_close_box ppf ()
  | Open_tag tag -> Format.pp_open_stag ppf tag
  | Close_tag -> Format.pp_close_stag ppf ()
  | Open_tbox -> Format.pp_open_tbox ppf ()
  | Tab_break {width;offset} -> Format.pp_print_tbreak ppf width offset
  | Set_tab -> Format.pp_set_tab ppf ()
  | Close_tbox -> Format.pp_close_tbox ppf ()
  | Simple_break {spaces;indent} -> Format.pp_print_break ppf spaces indent
  | Break {fits;breaks} -> Format.pp_print_custom_break ppf ~fits ~breaks
  | Flush {newline=true} -> Format.pp_print_newline ppf ()
  | Flush {newline=false} -> Format.pp_print_flush ppf ()
  | Newline -> Format.pp_force_newline ppf ()
  | If_newline -> Format.pp_print_if_newline ppf ()
  | With_size _ -> ()

let rec interpret ppf = function
  | [] -> ()
  | With_size n :: Data x :: q ->
      Format.pp_print_as ppf n x;
      interpret ppf q
  | x :: q ->
      interpret_elt ppf x;
      interpret ppf q

let format ppf doc = interpret ppf (to_list doc)



let box_type =
  let open CamlinternalFormatBasics in
  function
  | Pp_fits -> H
  | Pp_hbox -> H
  | Pp_vbox -> V
  | Pp_hovbox -> HoV
  | Pp_hvbox -> HV
  | Pp_box -> B


let format_open_box_gen ppf kind indent =
  match kind with
  | H-> Format.pp_open_hbox ppf ()
  | V -> Format.pp_open_vbox ppf indent
  | HV -> Format.pp_open_hvbox ppf indent
  | HoV -> Format.pp_open_hovbox ppf indent
  | B -> Format.pp_open_box ppf indent
