(* Split a scope on the dot. *)
let split_scope spec =
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
  | scope :: _ -> split_scope scope, text

let rec get_styles style classes =
  match classes with
  | [] -> [ANSITerminal.Reset]
  | class_ :: classes ->
     match style class_ with
     | Some styles -> styles
     | None -> get_styles style classes

let print_span style (classes, str) =
  ANSITerminal.print_string (get_styles style classes) str

let print_line style spans =
  List.iter (print_span style) spans

let print_block style lines =
  List.iter (print_line style) lines

let create_span scopes i j line =
  assert (j >= i);
  let inner_text = String.sub line i (j - i) in
  create_span scopes inner_text

let rec highlight_tokens i acc line = function
  | [] -> List.rev acc
  | tok :: toks ->
     let span = create_span tok.TmLanguage.scopes i tok.ending line in
     highlight_tokens tok.ending (span :: acc) line toks

let highlight_line t grammar stack line =
  (* Some patterns don't work if there isn't a newline *)
  let line = line ^ "\n" in
  let tokens, stack = TmLanguage.tokenize_exn t grammar stack line in
  let spans = highlight_tokens 0 [] line tokens in
  spans, stack

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
  | "entity.name.tag.label" -> Some [ANSITerminal.cyan]
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
  | _ -> None

let () =
  Printexc.record_backtrace true;
  if Array.length Sys.argv < 3 then (
    prerr_endline "No language specified.";
    exit 1
  ) else
    let source = Sys.argv.(1) in
    let t = TmLanguage.create () in
    for i = 2 to Array.length Sys.argv - 1 do
      let chan = open_in Sys.argv.(i) in
      let plist =
        Fun.protect (fun () -> Markup.channel chan |> Plist_xml.parse_exn)
          ~finally:(fun () -> close_in chan)
      in
      let grammar = TmLanguage.of_plist_exn plist in
      TmLanguage.add_grammar t grammar
    done;
    match TmLanguage.find_by_name t source with
    | None ->
       prerr_endline ("Unknown language " ^ source);
       exit 1
    | Some grammar ->
       read t grammar TmLanguage.empty |> print_block style
