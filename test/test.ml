let read_file filename =
  let chan = open_in filename in
  Fun.protect (fun () ->
      let buf = Buffer.create 256 in
      let rec loop () =
        match input_line chan with
        | exception End_of_file -> Buffer.contents buf
        | line ->
          Buffer.add_string buf line;
          Buffer.add_char buf '\n';
          loop ()
      in loop())
    ~finally:(fun () -> close_in chan)

let read_plist filename =
  let chan = open_in filename in
  let plist =
    Fun.protect (fun () -> Markup.channel chan |> Plist_xml.parse_exn)
      ~finally:(fun () -> close_in chan)
  in
  TmLanguage.of_plist_exn plist

let read_yojson_basic filename =
  TmLanguage.of_yojson_exn (Yojson.Basic.from_file filename)

let read_yaml filename =
  TmLanguage.of_ezjsonm_exn (Yaml.of_string_exn (read_file filename))

let t = TmLanguage.create ()

let () =
  TmLanguage.add_grammar t (read_yojson_basic "data/grammar1.json")

let tested_type =
  Alcotest.(list (pair int (list string)))

let test1 () =
  let open TmLanguage in
  let check expected s =
    let toks, _ =
      tokenize_exn t (Option.get (find_by_scope_name t "source.a")) empty s
    in
    let toks = List.map (fun tok -> (ending tok, scopes tok)) toks in
    Alcotest.check tested_type s expected toks
  in
  check [ 1, ["keyword.letter"; "source.a"] ] "a";
  check [ 1, ["keyword.letter"; "source.a"]
        ; 2, ["punctuation.paren.open"; "source.a"]
        ; 3, ["keyword.letter"; "expression.group"; "source.a"]
        ; 4, ["punctuation.paren.close"; "source.a"] ] "a(a)"

let () =
  let open Alcotest in
  run "Suite1" [
      "", [
          test_case "" `Quick test1;
        ]
    ]
