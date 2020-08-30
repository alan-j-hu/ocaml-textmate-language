open Tm_highlight

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

let create_span scopes text =
  match scopes with
  | [] -> [], text
  | classes :: _ -> get_classes classes, text

let create_line spans = spans

let create_block lines = lines

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

let create_span scopes i j line =
  assert (j >= i);
  let inner_text = String.sub line i (j - i) in
  create_span scopes inner_text

let rec highlight_tokens i acc line = function
  | [] -> List.rev acc
  | tok :: toks ->
     let span = create_span tok.scopes i tok.ending line in
     highlight_tokens tok.ending (span :: acc) line toks

(** Maps over the list while keeping track of some state.

    Discards the state because I don't need it. *)
let rec map_fold f acc = function
  | [] -> []
  | x :: xs ->
     let y, acc = f acc x in
     y :: map_fold f acc xs

let highlight_line t grammar stack line =
  (* Some patterns don't work if there isn't a newline *)
  let line = line ^ "\n" in
  let tokens, stack = tokenize_line t grammar stack line in
  let spans = highlight_tokens 0 [] line tokens in
  create_line spans, stack

let highlight_block t grammar code =
  let lines = String.split_on_char '\n' code in
  let a's = map_fold (highlight_line t grammar) empty lines in
  create_block a's

let read t grammar stack =
  let rec loop stack acc =
    match read_line () with
    | exception End_of_file -> List.rev acc
    | line ->
       let line, stack = highlight_line t grammar stack line in
       loop stack (line :: acc)
  in loop stack []

(* Color styles *)
let style = function
  | "constant.character" -> Some [ANSITerminal.magenta]
  | "constant.language.boolean" -> Some [ANSITerminal.cyan]
  | "comment" -> Some [ANSITerminal.magenta]
  | "constant.numeric" -> Some [ANSITerminal.blue]
  | "entity.name.type.variant" -> Some [ANSITerminal.Bold]
  | "invalid" -> Some [ANSITerminal.red]
  | "keyword.control" -> Some [ANSITerminal.magenta]
  | "keyword.operator" -> Some [ANSITerminal.yellow]
  | "keyword" -> Some [ANSITerminal.Bold]
  | "support.other.module" -> Some [ANSITerminal.green]
  | "meta.module-reference" -> Some [ANSITerminal.green]
  | "punctuation.definition.comment" -> Some [ANSITerminal.cyan]
  | "punctuation.definition.string" -> Some [ANSITerminal.magenta]
  | "string.quoted" -> Some [ANSITerminal.magenta]
  | "variable.parameter.labeled" | "variable.parameter.optional" ->
     Some [ANSITerminal.cyan]
  | _ -> None

let () =
  Printexc.record_backtrace true;
  if Array.length Sys.argv < 3 then (
    prerr_endline "No grammar file specified.";
    exit 1
  ) else
    let source = Sys.argv.(1) in
    let t = Tm_highlight.create () in
    for i = 2 to Array.length Sys.argv - 1 do
      let chan = open_in Sys.argv.(i) in
      let plist =
        Fun.protect (fun () -> Markup.channel chan |> Plist_xml.parse_exn)
          ~finally:(fun () -> close_in chan)
      in
      let grammar = Tm_highlight.of_plist_exn plist in
      Tm_highlight.add_grammar t grammar
    done;
    match Tm_highlight.find_by_name t source with
    | None ->
       prerr_endline ("Unknown language " ^ source);
       exit 1
    | Some grammar ->
       read t grammar Tm_highlight.empty
       |> create_block
       |> print_block style ANSITerminal.print_string
