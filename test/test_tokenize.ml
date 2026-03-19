open Util

let toks_to_list toks =
  List.map (fun tok -> (TmLanguage.ending tok, TmLanguage.scopes tok)) toks

let make_grammar json =
  let grammar = TmLanguage.of_yojson_exn json in
  let t = TmLanguage.create () in
  TmLanguage.add_grammar t grammar;
  (t, grammar)

let ganchor_grammar ~scope_name ~end_pattern : Yojson.Basic.t =
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

let parent_ganchor_grammar : Yojson.Basic.t =
  `Assoc
    [
      ("scopeName", `String "source.tmtest");
      ("name", `String "Test Lang");
      ( "patterns",
        `List
          [
            `Assoc
              [
                ("name", `String "source.test-lang");
                ("begin", `String "\\(");
                ("end", `String "\\G\\)");
                ( "beginCaptures",
                  `Assoc
                    [
                      ( "0",
                        `Assoc
                          [ ("name", `String "punctuation.parenthesis.open") ]
                      );
                    ] );
                ( "endCaptures",
                  `Assoc
                    [
                      ( "0",
                        `Assoc
                          [ ("name", `String "punctuation.parenthesis.close") ]
                      );
                    ] );
                ( "patterns",
                  `List
                    [
                      `Assoc
                        [
                          ("name", `String "keyword.control.test-lang");
                          ("match", `String "\\GA");
                        ];
                      `Assoc
                        [
                          ("name", `String "keyword.control.test-lang");
                          ("match", `String "\\GB");
                        ];
                    ] );
              ];
          ] );
      ("repository", `Assoc []);
    ]

let ganchor_while_grammar ~scope_name ~while_pattern : Yojson.Basic.t =
  `Assoc
    [
      ("scopeName", `String scope_name);
      ("name", `String "ganchor-while");
      ( "patterns",
        `List
          [
            `Assoc
              [
                ("begin", `String "A");
                ("while", `String while_pattern);
                ("name", `String "while.test");
              ];
            `Assoc
              [ ("match", `String "[a-zA-Z]+"); ("name", `String "word.test") ];
          ] );
    ]

let check_end_pattern_g_anchor () =
  let t, grammar =
    make_grammar
      (ganchor_grammar ~scope_name:"source.ganchor" ~end_pattern:"(?<!\\G)\"")
  in
  let toks, _ =
    TmLanguage.tokenize_exn t grammar TmLanguage.empty "cmd \"x\" -y"
  in
  Alcotest.(check (list (pair int (list string))))
    "g-anchor end pattern"
    [
      (3, [ "word.test"; "source.ganchor" ]);
      (4, [ "source.ganchor" ]);
      (5, [ "string.quoted.test"; "source.ganchor" ]);
      (6, [ "string.quoted.test"; "source.ganchor" ]);
      (7, [ "string.quoted.test"; "string.quoted.test"; "source.ganchor" ]);
      (8, [ "source.ganchor" ]);
      (10, [ "word.test"; "source.ganchor" ]);
    ]
    (toks_to_list toks)

let check_positive_g_anchor_fails_without_parent_anchor () =
  let t, grammar =
    make_grammar
      (ganchor_grammar ~scope_name:"source.ganchor.pos" ~end_pattern:"\\G\"")
  in
  let _, stack = TmLanguage.tokenize_exn t grammar TmLanguage.empty "\"" in
  let toks, _ = TmLanguage.tokenize_exn t grammar stack "\"x" in
  (* \G end pattern can't match without parent anchor, so string stays open *)
  Alcotest.(check (list (pair int (list string))))
    "positive \\G end fails without parent anchor"
    [
      (2, [ "string.quoted.test"; "string.quoted.test"; "source.ganchor.pos" ]);
    ]
    (toks_to_list toks)

let check_negative_g_anchor_matches_without_parent_anchor () =
  let t, grammar =
    make_grammar
      (ganchor_grammar ~scope_name:"source.ganchor.neg"
         ~end_pattern:"(?<!\\G)\"")
  in
  let _, stack = TmLanguage.tokenize_exn t grammar TmLanguage.empty "\"" in
  let toks, _ = TmLanguage.tokenize_exn t grammar stack "\"x" in
  (* Negative \G matches when anchor is unavailable, so string closes *)
  Alcotest.(check (list (pair int (list string))))
    "negative \\G end matches without parent anchor"
    [
      (1, [ "string.quoted.test"; "string.quoted.test"; "source.ganchor.neg" ]);
      (2, [ "word.test"; "source.ganchor.neg" ]);
    ]
    (toks_to_list toks)

let check_parent_g_anchor_in_nested_patterns () =
  let t, grammar = make_grammar parent_ganchor_grammar in
  let toks, _ = TmLanguage.tokenize_exn t grammar TmLanguage.empty "(AB)" in
  (* A matches \GA at anchor pos, B and ) don't match \G patterns *)
  Alcotest.(check (list (pair int (list string))))
    "nested patterns honor parent \\G anchor"
    [
      (1, [ "punctuation.parenthesis.open"; "source.tmtest" ]);
      (2, [ "keyword.control.test-lang"; "source.test-lang"; "source.tmtest" ]);
      (4, [ "source.test-lang"; "source.test-lang"; "source.tmtest" ]);
    ]
    (toks_to_list toks)

let check_parent_g_anchor_closes_on_empty_content () =
  let t, grammar = make_grammar parent_ganchor_grammar in
  let toks, _ = TmLanguage.tokenize_exn t grammar TmLanguage.empty "()" in
  (* \G end matches immediately at anchor position, closing the group *)
  Alcotest.(check (list (pair int (list string))))
    "empty content closes with parent \\G anchor"
    [
      (1, [ "punctuation.parenthesis.open"; "source.tmtest" ]);
      (2, [ "punctuation.parenthesis.close"; "source.tmtest" ]);
    ]
    (toks_to_list toks)

let check_positive_g_anchor_while_fails_without_anchor () =
  let t, grammar =
    make_grammar
      (ganchor_while_grammar ~scope_name:"source.ganchor.while.pos"
         ~while_pattern:"\\G.")
  in
  let _, stack = TmLanguage.tokenize_exn t grammar TmLanguage.empty "A" in
  let toks, _ = TmLanguage.tokenize_exn t grammar stack "x" in
  (* \G while can't match without anchor, so while-scope is exited *)
  Alcotest.(check (list (pair int (list string))))
    "positive \\G while fails without anchor"
    [ (1, [ "word.test"; "source.ganchor.while.pos" ]) ]
    (toks_to_list toks)

let check_negative_g_anchor_while_matches_without_anchor () =
  let t, grammar =
    make_grammar
      (ganchor_while_grammar ~scope_name:"source.ganchor.while.neg"
         ~while_pattern:"(?<!\\G).")
  in
  let _, stack = TmLanguage.tokenize_exn t grammar TmLanguage.empty "A" in
  let toks, _ = TmLanguage.tokenize_exn t grammar stack "x" in
  (* Negative \G while matches when anchor unavailable, so while-scope stays *)
  Alcotest.(check (list (pair int (list string))))
    "negative \\G while matches without anchor"
    [ (1, [ "while.test"; "source.ganchor.while.neg" ]) ]
    (toks_to_list toks)

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
        [
          [ { line = "a"; expected = [ (1, [ "source.zero-width-loop" ]) ] } ];
        ];
      test_tokenize_json "data/zero_width_end_loop.json"
        "source.zero-width-end-loop"
        [
          [
            {
              line = "a";
              expected = [ (1, [ "source.zero-width-end-loop" ]) ];
            };
            {
              line = "z";
              expected = [ (1, [ "source.zero-width-end-loop" ]) ];
            };
          ];
        ];
      test_tokenize_json "data/zero_width_match_loop.json"
        "source.zero-width-match-loop"
        [
          [
            {
              line = "a";
              expected = [ (1, [ "source.zero-width-match-loop" ]) ];
            };
          ];
        ];
      ( "g-anchor-end-pattern",
        [
          Alcotest.test_case "Closes quoted scope after begin anchor" `Quick
            check_end_pattern_g_anchor;
          Alcotest.test_case "Positive \\G end fails without parent anchor"
            `Quick check_positive_g_anchor_fails_without_parent_anchor;
          Alcotest.test_case "Negative \\G end matches without parent anchor"
            `Quick check_negative_g_anchor_matches_without_parent_anchor;
        ] );
      ( "g-anchor-parent-and-while",
        [
          Alcotest.test_case "Nested patterns use parent \\G anchor" `Quick
            check_parent_g_anchor_in_nested_patterns;
          Alcotest.test_case "Empty content closes with parent \\G anchor"
            `Quick check_parent_g_anchor_closes_on_empty_content;
          Alcotest.test_case "Positive \\G while fails without anchor" `Quick
            check_positive_g_anchor_while_fails_without_anchor;
          Alcotest.test_case "Negative \\G while matches without anchor" `Quick
            check_negative_g_anchor_while_matches_without_anchor;
        ] );
    ]
