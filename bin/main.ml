module H = Tm_highlight.Make(Tm_highlight_html.Renderer)

let () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "No grammar file specified.";
    exit 1
  ) else
    let chan = open_in Sys.argv.(1) in
    let plist =
      Fun.protect (fun () -> Markup.channel chan |> Plist_xml.parse_exn)
        ~finally:(fun () -> close_in chan)
    in
    let grammar = Tm_highlight.of_plist_exn plist in
    read_line ()
    |> H.highlight_block grammar
    |> Soup.pretty_print
    |> print_endline
