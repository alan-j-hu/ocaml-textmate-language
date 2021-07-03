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

let read_ezjsonm filename =
  let chan = open_in filename in
  let json =
    Fun.protect (fun () -> Ezjsonm.from_channel chan)
      ~finally:(fun () -> close_in chan)
  in
  TmLanguage.of_ezjsonm_exn json

let check data name cases () =
  let open TmLanguage in
  let t = create () in
  add_grammar t data;
  let tested_type = Alcotest.(list (pair int (list string))) in
  let check lines =
    ignore (List.fold_left (fun stack (expected, str) ->
        let toks, stack =
          tokenize_exn t (Option.get (find_by_scope_name t name)) stack str
        in
        let toks = List.map (fun tok -> (ending tok, scopes tok)) toks in
        Alcotest.check tested_type str expected toks;
        stack) empty lines)
  in
  List.iter check cases

let test filename name cases =
  ( name
  , [ Alcotest.test_case
        "Yojson" `Quick (check (read_yojson_basic filename) name cases)
    ; Alcotest.test_case
        "Ezjsonm" `Quick (check (read_ezjsonm filename) name cases) ] )

let () =
  Alcotest.run "Suite1"
    [ test "data/a.json" "source.a"
        [ [ [ 1, ["keyword.letter"; "source.a"] ],
            "a" ]
        ; [ [ 1, ["keyword.letter"; "source.a"]
            ; 2, ["punctuation.paren.open"; "source.a"]
            ; 3, ["keyword.letter"; "expression.group"; "source.a"]
            ; 4, ["punctuation.paren.close"; "source.a"] ],
            "a(a)" ]
        ; [ [ 1, ["keyword.letter"; "source.a"]
            ; 2, ["punctuation.paren.open"; "source.a"] ],
            "a("
          ; [ 1, ["keyword.letter"; "expression.group"; "source.a" ]
            ; 2, ["punctuation.paren.close"; "source.a"] ],
            "a)" ] ]
    ; test "data/while.json" "source.while"
        [ [ [ 1, ["begin"; "source.while"] ],
            "a" ]
        ; [ [ 1, ["begin"; "source.while"]
            ; 2, ["expression.group"; "source.while"] ],
            "ac"
          ; [ 1, ["while"; "source.while"]
            ; 2, ["keyword.letter"; "expression.group"; "source.while"] ],
            "bc" ] ] ]
