module Make (R : Tm_highlight.Renderer) = struct
  module H = Tm_highlight.Make(R)

  let read grammar t =
    let rec loop t acc =
      match read_line () with
      | exception End_of_file -> List.rev acc
      | line ->
         let line, t = H.highlight_line grammar t line in
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
  | "string" -> Some [ANSITerminal.magenta]
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
