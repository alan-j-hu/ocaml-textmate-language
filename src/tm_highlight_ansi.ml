type span = string list * string
type line = span list
type block = line list
type style = string -> ANSITerminal.style list option
type printer = ANSITerminal.style list -> string -> unit

let get_classes spec =
  let toks = String.split_on_char '.' spec in
  let _, classes =
    List.fold_left (fun (toks, classes) tok ->
        let toks = tok :: toks in
        (toks, (String.concat "." (List.rev toks)) :: classes)
      ) ([], []) toks
  in classes

module Renderer = struct
  type nonrec span = span
  type nonrec line = line
  type nonrec block = block

  let create_span name text =
    match name with
    | None -> [], text
    | Some classes -> get_classes classes, text

  let create_line spans = spans

  let create_block lines = lines
end

let rec get_styles style classes =
  match classes with
  | [] -> [ANSITerminal.Reset]
  | class_ :: classes ->
     match style class_ with
     | Some styles -> styles
     | None -> get_styles style classes

let print_span style printer (classes, str) =
  printer (get_styles style classes) str

let print_line style printer spans =
  List.iter (print_span style printer) spans

let print_block style printer lines =
  List.iter (print_line style printer) lines
