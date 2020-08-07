type span = ANSITerminal.style list * string
type line = span list
type block = line list
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
    let classes = Option.map get_classes name in
    match classes with
    | None -> ([ANSITerminal.Reset], text)
    | Some _ -> ([ANSITerminal.Foreground Blue], text)

  let create_line spans = spans

  let create_block lines = lines
end

let print_span printer (styles, str) =
  printer styles str

let print_line printer spans =
  List.iter (print_span printer) spans;
  printer [] "\n"

let print_block printer lines =
  List.iter (print_line printer) lines
