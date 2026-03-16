open Util

let one_token line scopes = [ { line; expected = [ (1, scopes) ] } ]
let line_token line scopes = { line; expected = [ (1, scopes) ] }

let check_end_pattern_g_anchor () =
  let grammar_json =
    Yojson.Basic.from_string
      {|{
  "scopeName": "source.ganchor",
  "name": "ganchor",
  "patterns": [
    {
      "begin": "\"",
      "end": "(?<!\\G)\"",
      "name": "string.quoted.test"
    },
    {
      "match": "[a-zA-Z-]+",
      "name": "word.test"
    }
  ]
}|}
  in
  let grammar = TmLanguage.of_yojson_exn grammar_json in
  let t = TmLanguage.create () in
  TmLanguage.add_grammar t grammar;
  let line = "cmd \"x\" -y" in
  let toks, _ = TmLanguage.tokenize_exn t grammar TmLanguage.empty line in
  let spans =
    let rec build start = function
      | [] -> []
      | tok :: rest ->
        let ending = TmLanguage.ending tok in
        let text = String.sub line start (ending - start) in
        (text, TmLanguage.scopes tok) :: build ending rest
    in
    build 0 toks
  in
  let dash_scopes =
    List.find_map
      (fun (text, scopes) ->
        if String.contains text '-' then Some scopes else None)
      spans
  in
  let has_scope scope scopes = List.exists (( = ) scope) scopes in
  let ok =
    match dash_scopes with
    | None -> false
    | Some scopes ->
      has_scope "word.test" scopes
      && not (has_scope "string.quoted.test" scopes)
  in
  Alcotest.(check bool)
    "dash token should be outside quoted string and matched as a word" true ok

let check_overlapping_begin_captures_opening_quote () =
  let grammar_json =
    Yojson.Basic.from_string
      {|{
  "scopeName": "source.overlap",
  "name": "overlap",
  "patterns": [
    {
      "begin": "((\"))",
      "end": "\"",
      "beginCaptures": {
        "1": {},
        "2": {
          "name": "string.quoted.double.test"
        }
      },
      "endCaptures": {
        "0": {
          "name": "string.quoted.double.test"
        }
      },
      "name": "meta.wrapper.test",
      "contentName": "string.quoted.double.test"
    }
  ]
}|}
  in
  let grammar = TmLanguage.of_yojson_exn grammar_json in
  let t = TmLanguage.create () in
  TmLanguage.add_grammar t grammar;
  let line = "\"x\"" in
  let toks, _ = TmLanguage.tokenize_exn t grammar TmLanguage.empty line in
  let spans =
    let rec build start = function
      | [] -> []
      | tok :: rest ->
        let ending = TmLanguage.ending tok in
        let text = String.sub line start (ending - start) in
        (text, TmLanguage.scopes tok) :: build ending rest
    in
    build 0 toks
  in
  let quote_scopes =
    List.fold_left
      (fun acc (text, scopes) -> if text = "\"" then scopes :: acc else acc)
      [] spans
    |> List.rev
  in
  let has_scope scope scopes = List.exists (( = ) scope) scopes in
  let ok =
    match quote_scopes with
    | [ opening; closing ] ->
      has_scope "string.quoted.double.test" opening
      && has_scope "string.quoted.double.test" closing
    | _ -> false
  in
  Alcotest.(check bool)
    "opening and closing quotes should both be string-scoped" true ok

let () =
  Alcotest.run "Highlighting"
    [
      test_tokenize_json "data/a.json" "source.a"
        [
          [
            {
              line = "a";
              expected = [ (1, [ "keyword.letter"; "source.a" ]) ];
            };
          ];
          [
            {
              line = "a(a)";
              expected =
                [
                  (1, [ "keyword.letter"; "source.a" ]);
                  (2, [ "punctuation.paren.open"; "source.a" ]);
                  (3, [ "keyword.letter"; "expression.group"; "source.a" ]);
                  (4, [ "punctuation.paren.close"; "source.a" ]);
                ];
            };
          ];
          [
            {
              line = "a(";
              expected =
                [
                  (1, [ "keyword.letter"; "source.a" ]);
                  (2, [ "punctuation.paren.open"; "source.a" ]);
                ];
            };
            {
              line = "a)";
              expected =
                [
                  (1, [ "keyword.letter"; "expression.group"; "source.a" ]);
                  (2, [ "punctuation.paren.close"; "source.a" ]);
                ];
            };
          ];
        ];
      test_tokenize_json "data/while.json" "source.while"
        [
          [ { line = "a"; expected = [ (1, [ "begin"; "source.while" ]) ] } ];
          [
            {
              line = "ac";
              expected =
                [
                  (1, [ "begin"; "source.while" ]);
                  (2, [ "expression.group"; "source.while" ]);
                ];
            };
            {
              line = "bc";
              expected =
                [
                  (1, [ "while"; "source.while" ]);
                  (2, [ "keyword.letter"; "expression.group"; "source.while" ]);
                ];
            };
          ];
        ];
      (* See https://github.com/microsoft/vscode-textmate/issues/25 *)
      test_tokenize_json "data/multiwhile.json" "source.multiwhile"
        [
          [
            {
              line = "X";
              expected = [ (1, [ "xbegin"; "source.multiwhile" ]) ];
            };
            {
              line = "xY";
              expected =
                [
                  (1, [ "xwhile"; "source.multiwhile" ]);
                  (2, [ "ybegin"; "xlist"; "source.multiwhile" ]);
                ];
            };
            {
              line = "yxy";
              expected =
                [
                  (1, [ "source.multiwhile" ]);
                  (2, [ "xwhile"; "source.multiwhile" ]);
                  (3, [ "ywhile"; "xlist"; "source.multiwhile" ]);
                ];
            };
            {
              line = "xy";
              expected =
                [
                  (1, [ "xwhile"; "source.multiwhile" ]);
                  (2, [ "ywhile"; "xlist"; "source.multiwhile" ]);
                ];
            };
            { line = "y"; expected = [ (1, [ "source.multiwhile" ]) ] };
          ];
        ];
      test_tokenize_json "data/groups.json" "source.groups"
        [
          [
            {
              line = "({#aaff59})";
              expected =
                [
                  (1, [ "punctuation.paren.open"; "source.groups" ]);
                  (2, [ "punctuation.paren.open.groups"; "source.groups" ]);
                  ( 3,
                    [ "keyword.operator"; "expression.group"; "source.groups" ]
                  );
                  ( 9,
                    [ "constant.numeric"; "expression.group"; "source.groups" ]
                  );
                  (10, [ "punctuation.paren.close.groups"; "source.groups" ]);
                  (11, [ "punctuation.paren.close"; "source.groups" ]);
                ];
            };
          ];
        ];
      test_tokenize_json "data/zero_width_loop.json" "source.zero-width-loop"
        [ one_token "a" [ "source.zero-width-loop" ] ];
      test_tokenize_json "data/zero_width_end_loop.json"
        "source.zero-width-end-loop"
        [
          [
            line_token "a" [ "source.zero-width-end-loop" ];
            line_token "z" [ "source.zero-width-end-loop" ];
          ];
        ];
      test_tokenize_json "data/zero_width_match_loop.json"
        "source.zero-width-match-loop"
        [ one_token "a" [ "source.zero-width-match-loop" ] ];
      ( "g-anchor-end-pattern",
        [
          Alcotest.test_case "Closes quoted scope after begin anchor" `Quick
            check_end_pattern_g_anchor;
        ] );
      ( "overlapping-begin-captures",
        [
          Alcotest.test_case "Keeps string scope on opening quote" `Quick
            check_overlapping_begin_captures_opening_quote;
        ] );
    ]
