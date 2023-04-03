open Doc

type ('a,'b) fmt = ('a, doc ref, unit,'b) format4

open Immutable
let rec update_acc doc (acc: (doc ref,unit) CamlinternalFormat.acc)  : unit =
  match acc with
  | Acc_formatting_lit (p, f) ->
    update_acc doc p;
    doc := output_formatting_lit f !doc
  | Acc_formatting_gen (p, Acc_open_tag acc') ->
    let tag_doc = ref empty in
    update_acc doc p;
    update_acc tag_doc acc';
    doc := open_tag (Format.String_tag (to_string !tag_doc)) !doc
  | Acc_formatting_gen (p, Acc_open_box acc') ->
    update_acc doc p;
    let tag_doc = ref empty in
    update_acc tag_doc acc';
    let box = to_string !tag_doc in
    let (indent, bty) = CamlinternalFormat.open_box_of_string box in
    doc := open_box (box_type bty) indent !doc
  | Acc_string_literal (p, s)
  | Acc_data_string (p, s)   ->
    update_acc doc p;
    doc := string s !doc
  | Acc_char_literal (p, c)
  | Acc_data_char (p, c)     -> update_acc doc p; doc := char c !doc
  | Acc_delay (p, f)         -> update_acc doc p; f doc
  | Acc_flush p              -> update_acc doc p;  doc :=  flush !doc
  | Acc_invalid_arg (p, msg) -> update_acc doc p; invalid_arg msg
  | End_of_acc               -> ()

let kprintf k rdoc (CamlinternalFormatBasics.Format (fmt, _))  =
  CamlinternalFormat.make_printf
    (fun acc -> update_acc rdoc acc; k rdoc)
    End_of_acc fmt
let printf doc fmt = kprintf ignore doc fmt
