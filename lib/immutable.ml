open Doc

let open_box kind indent doc = add (Open_box {kind;indent}) doc
let close_box doc = add Close_box doc

let string s doc = add (Data s) doc
let bytes b doc = add (Data (Bytes.to_string b)) doc
let with_size n doc = add (With_size n) doc

let int n doc = add (Data (string_of_int n)) doc
let float f doc = add (Data (string_of_float f)) doc
let char c doc = add (Data (String.make 1 c)) doc
let bool c doc = add (Data (Bool.to_string c)) doc


let break ~spaces ~indent doc = add (Simple_break {spaces; indent}) doc
let space doc = break ~spaces:1 ~indent:0 doc
let cut = break ~spaces:0 ~indent:0

let custom_break ~fits ~breaks doc = add (Break {fits;breaks}) doc

let force_newline doc = add Newline doc
let if_newline doc = add If_newline doc

let flush doc = add (Flush {newline=false}) doc
let force_stop = add (Flush {newline=true})

let open_tbox doc = add Open_tbox doc
let set_tab doc = add Set_tab doc
let tab_break ~width ~offset doc = add (Tab_break {width;offset}) doc
let tab doc = tab_break ~width:0 ~offset:0 doc
let close_tbox doc = add Close_tbox doc

let open_tag stag doc = add (Open_tag stag) doc
let close_tag doc = add Close_tag doc

let rec list ?(sep=Fun.id) elt l doc = match l with
  | [] -> doc
  | [a] -> elt a doc
  | a :: (_ :: _ as q) ->
    doc |> elt a |> sep |> list ~sep elt q


let option ?(none=Fun.id) elt o doc = match o with
  | None -> none doc
  | Some x -> elt x doc

(* To format free-flowing text *)
let rec subtext len left right s doc =
  let flush doc =
    doc |> string (String.sub s left (right - left))
  in
  let after_flush doc = subtext len (right+1) (right+1) s doc in
  if right = len then
    if left <> len then flush doc else doc
  else
    match s.[right] with
    | '\n' ->
      doc |> flush |> force_newline |> after_flush
    | ' ' ->
      doc |> flush |> space |> after_flush
    (* there is no specific support for '\t'
       as it is unclear what a right semantics would be *)
    | _ -> subtext len left (right + 1) s doc


let text s doc =
  subtext (String.length s) 0 0 s doc

type ('a,'b) fmt = ('a, doc, doc, 'b) format4
type printer = doc -> doc




let output_formatting_lit fmting_lit doc =
  let open CamlinternalFormatBasics in
  match fmting_lit with
  | Close_box    -> close_box doc
  | Close_tag                 -> close_tag doc
  | Break (_, width, offset)  -> break ~spaces:width ~indent:offset doc
  | FFlush                    -> flush doc
  | Force_newline             -> force_newline doc
  | Flush_newline             -> force_stop doc
  | Magic_size (_, n)         -> with_size n doc
  | Escaped_at                -> char '@' doc
  | Escaped_percent           -> char '%' doc
  | Scan_indic c              -> doc |> char '@' |> char c

let to_string doc =
  let b = Buffer.create 20 in
  let convert = function
    | Data s -> Buffer.add_string b s
    | _ -> ()
  in
  fold (fun () x -> convert x) () doc;
  Buffer.contents b

let rec compose_acc acc doc =
  let open CamlinternalFormat in
  match acc with
  | CamlinternalFormat.Acc_formatting_lit (p, f) ->
    doc |> compose_acc p |> output_formatting_lit f
  | Acc_formatting_gen (p, Acc_open_tag acc') ->
    let tag = to_string (compose_acc acc' empty) in
    let doc = compose_acc p doc in
    doc |> open_tag (Format.String_tag tag)
  | Acc_formatting_gen (p, Acc_open_box acc') ->
    let doc = compose_acc p doc in
    let box = to_string (compose_acc acc' empty) in
    let (indent, bty) = CamlinternalFormat.open_box_of_string box in
    doc |> open_box (box_type bty) indent
  | Acc_string_literal (p, s)
  | Acc_data_string (p, s)   ->
    doc |> compose_acc p |> string s
  | Acc_char_literal (p, c)
  | Acc_data_char (p, c)     -> doc |> compose_acc p |> char c
  | Acc_delay (p, f)         -> doc |> compose_acc p |> f
  | Acc_flush p              -> doc |> compose_acc p |> flush
  | Acc_invalid_arg (_p, msg) ->  invalid_arg msg;
  | End_of_acc               -> doc

let kprintf k (CamlinternalFormatBasics.Format (fmt, _))  =
  CamlinternalFormat.make_printf
    (fun acc doc -> doc |> compose_acc acc |> k )
    End_of_acc fmt

let printf doc = kprintf Fun.id doc
let kmsg k  (CamlinternalFormatBasics.Format (fmt, _)) =
  CamlinternalFormat.make_printf
    (fun acc -> k (compose_acc acc empty))
    End_of_acc fmt

let msg fmt = kmsg Fun.id fmt
