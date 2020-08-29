open Tm_highlight

module type S = sig
  type line
  type block

  val highlight_line : grammar -> t -> string -> line * t
  val highlight_block : grammar -> string -> block
end

module Make (R : Renderer) = struct
  type line = R.line
  type block = R.block

  let create_span name i j line =
    assert (j >= i);
    let inner_text = String.sub line i (j - i) in
    R.create_span name inner_text

  let rec highlight_tokens i acc line = function
    | [] -> List.rev acc
    | tok :: toks ->
       let span = create_span tok.scope i tok.ending line in
       highlight_tokens tok.ending (span :: acc) line toks

  (** Maps over the list while keeping track of some state.

      Discards the state because I don't need it. *)
  let rec map_fold f acc = function
    | [] -> []
    | x :: xs ->
       let y, acc = f acc x in
       y :: map_fold f acc xs

  let highlight_line grammar stack line =
    (* Some patterns don't work if there isn't a newline *)
    let line = line ^ "\n" in
    let tokens, stack = tokenize_line grammar stack line in
    let spans = highlight_tokens 0 [] line tokens in
    R.create_line spans, stack

  let highlight_block grammar code =
    let lines = String.split_on_char '\n' code in
    let a's = map_fold (highlight_line grammar) empty lines in
    R.create_block a's

  let read grammar t =
    let rec loop t acc =
      match read_line () with
      | exception End_of_file -> List.rev acc
      | line ->
         let line, t = highlight_line grammar t line in
         loop t (line :: acc)
    in loop t []
end

module Ansi = Make(Tm_highlight_ansi.Renderer)
module Html = Make(Tm_highlight_html.Renderer)

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
    let mode = Sys.argv.(1) in
    let chan = open_in Sys.argv.(2) in
    let plist =
      Fun.protect (fun () -> Markup.channel chan |> Plist_xml.parse_exn)
        ~finally:(fun () -> close_in chan)
    in
    let grammar = Tm_highlight.of_plist_exn plist in
    match mode with
    | "ansi" ->
       Ansi.read grammar Tm_highlight.empty
       |> Tm_highlight_ansi.Renderer.create_block
       |> Tm_highlight_ansi.print_block style ANSITerminal.print_string
    | "html" ->
       Html.read grammar Tm_highlight.empty
       |> Tm_highlight_html.Renderer.create_block
       |> Soup.pretty_print
       |> print_endline
    | _ ->
       prerr_endline "Invalid mode: Must either be ansi or html";
       exit 1
