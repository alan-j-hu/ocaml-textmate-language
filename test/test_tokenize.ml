open Util

let one_token line scopes = [ { line; expected = [ (1, scopes) ] } ]
let line_token line scopes = { line; expected = [ (1, scopes) ] }
let has_scope scope scopes = List.exists (( = ) scope) scopes

let scopes_for_char line toks ch =
  let rec build start = function
    | [] -> []
    | tok :: rest ->
      let ending = TmLanguage.ending tok in
      let text = String.sub line start (ending - start) in
      (text, TmLanguage.scopes tok) :: build ending rest
  in
  List.find_map
    (fun (text, scopes) ->
      if String.contains text ch then Some scopes else None)
    (build 0 toks)

let create_ganchor_grammar ~scope_name ~end_pattern =
  let grammar_json : Yojson.Basic.t =
    `Assoc
      [
        ("scopeName", `String scope_name);
        ("name", `String "ganchor");
        ( "patterns",
          `List
            [
              `Assoc
                [
                  ("begin", `String "\"");
                  ("end", `String end_pattern);
                  ("name", `String "string.quoted.test");
                ];
              `Assoc
                [
                  ("match", `String "[a-zA-Z-]+"); ("name", `String "word.test");
                ];
            ] );
      ]
  in
  let grammar = TmLanguage.of_yojson_exn grammar_json in
  let t = TmLanguage.create () in
  TmLanguage.add_grammar t grammar;
  (t, grammar)

let check_end_pattern_g_anchor () =
  let t, grammar =
    create_ganchor_grammar ~scope_name:"source.ganchor"
      ~end_pattern:"(?<!\\G)\""
  in
  let line = "cmd \"x\" -y" in
  let toks, _ = TmLanguage.tokenize_exn t grammar TmLanguage.empty line in
  let dash_scopes = scopes_for_char line toks '-' in
  let ok =
    match dash_scopes with
    | None -> false
    | Some scopes ->
      has_scope "word.test" scopes
      && not (has_scope "string.quoted.test" scopes)
  in
  Alcotest.(check bool)
    "dash token should be outside quoted string and matched as a word" true ok

let check_g_anchor_without_parent_anchor ~scope_name ~end_pattern
    ~expect_word_scope () =
  let t, grammar = create_ganchor_grammar ~scope_name ~end_pattern in
  let _, stack = TmLanguage.tokenize_exn t grammar TmLanguage.empty "\"" in
  let line = "\"x" in
  let toks, _ = TmLanguage.tokenize_exn t grammar stack line in
  let x_scopes = scopes_for_char line toks 'x' in
  let ok =
    match x_scopes with
    | None -> false
    | Some scopes ->
      has_scope "word.test" scopes = expect_word_scope
      && has_scope "string.quoted.test" scopes <> expect_word_scope
  in
  Alcotest.(check bool)
    "unexpected scope for unavailable parent anchor" true ok

let check_positive_g_anchor_fails_without_parent_anchor () =
  check_g_anchor_without_parent_anchor
    ~scope_name:"source.ganchor.positive-unavailable" ~end_pattern:"\\G\""
    ~expect_word_scope:false ()

let check_negative_g_anchor_matches_without_parent_anchor () =
  check_g_anchor_without_parent_anchor
    ~scope_name:"source.ganchor.negative-unavailable" ~end_pattern:"(?<!\\G)\""
    ~expect_word_scope:true ()

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
          Alcotest.test_case "Positive \\G end fails without parent anchor"
            `Quick check_positive_g_anchor_fails_without_parent_anchor;
          Alcotest.test_case "Negative \\G end matches without parent anchor"
            `Quick check_negative_g_anchor_matches_without_parent_anchor;
        ] );
    ]
