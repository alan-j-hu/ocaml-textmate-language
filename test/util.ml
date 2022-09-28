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

type test_line = {
  line : string;
  expected : (int * string list) list
}

let check_find grammar scope_name filetypes () =
  let open TmLanguage in
  let t = create () in
  let check p =
    Alcotest.check
      Alcotest.bool scope_name true
      (p (TmLanguage.find_by_scope_name t scope_name));
    List.iter (fun filetype ->
        Alcotest.check
          Alcotest.bool filetype true
          (p (TmLanguage.find_by_filetype t filetype));
      ) filetypes
  in
  check ((=) None);
  add_grammar t grammar;
  check begin function
    | None -> false
    | Some grammar' -> grammar == grammar'
  end

let test_find filename scope_name filetypes =
  ( filename
  , [ Alcotest.test_case
        "Yojson"
        `Quick
        (check_find (read_yojson_basic filename) scope_name filetypes)
    ; Alcotest.test_case
        "Ezjsonm"
        `Quick
        (check_find (read_ezjsonm filename) scope_name filetypes) ] )


let check_tokenize grammar name cases () =
  let open TmLanguage in
  let t = create () in
  add_grammar t grammar;
  let tested_type = Alcotest.(list (pair int (list string))) in
  let check lines =
    ignore (List.fold_left (fun stack { line; expected } ->
        let toks, stack =
          tokenize_exn t (Option.get (find_by_scope_name t name)) stack line
        in
        let toks = List.map (fun tok -> (ending tok, scopes tok)) toks in
        Alcotest.check tested_type line expected toks;
        stack) empty lines)
  in
  List.iter check cases

let test_tokenize filename scope_name cases =
  ( filename
  , [ Alcotest.test_case
        "Yojson"
        `Quick
        (check_tokenize (read_yojson_basic filename) scope_name cases)
    ; Alcotest.test_case
        "Ezjsonm"
        `Quick
        (check_tokenize (read_ezjsonm filename) scope_name cases) ] )
