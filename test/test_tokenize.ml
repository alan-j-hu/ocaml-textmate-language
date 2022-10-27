open Util

let () =
  Alcotest.run "Highlighting" [
    test_tokenize "data/a.json" "source.a" [
      [
        { line = "a"
        ; expected = [ 1, ["keyword.letter"; "source.a"] ] }
      ];
      [
        { line = "a(a)"
        ; expected =
            [ 1, ["keyword.letter"; "source.a"]
            ; 2, ["punctuation.paren.open"; "source.a"]
            ; 3, ["keyword.letter"; "expression.group"; "source.a"]
            ; 4, ["punctuation.paren.close"; "source.a"] ] }
      ];
      [
        { line = "a("
        ; expected =
            [ 1, ["keyword.letter"; "source.a"]
            ; 2, ["punctuation.paren.open"; "source.a"] ] };
        { line = "a)"
        ; expected =
            [ 1, ["keyword.letter"; "expression.group"; "source.a"]
            ; 2, ["punctuation.paren.close"; "source.a"] ] }
      ]
    ];
    test_tokenize "data/while.json" "source.while" [
      [
        { line = "a"
        ; expected = [ 1, ["begin"; "source.while"] ] }
      ];
      [
        { line = "ac"
        ; expected =
            [ 1, ["begin"; "source.while"]
            ; 2, ["expression.group"; "source.while"] ] };
        { line = "bc"
        ; expected =
            [ 1, ["while"; "source.while"]
            ; 2, ["keyword.letter"; "expression.group"; "source.while"] ] }
      ]
    ];
    (* See https://github.com/microsoft/vscode-textmate/issues/25 *)
    test_tokenize "data/multiwhile.json" "source.multiwhile" [
      [
        { line = "X"
        ; expected = [ 1, ["xbegin"; "source.multiwhile"] ] };
        { line = "xY"
        ; expected =
            [ 1, ["xwhile"; "source.multiwhile"]
            ; 2, ["ybegin"; "xlist"; "source.multiwhile"] ] };
        { line = "yxy"
        ; expected =
            [ 1, ["source.multiwhile"]
            ; 2, ["xwhile"; "source.multiwhile"]
            ; 3, ["ywhile"; "xlist"; "source.multiwhile"] ] };
        { line = "xy"
        ; expected =
            [ 1, ["xwhile"; "source.multiwhile"]
            ; 2, ["ywhile"; "xlist"; "source.multiwhile"] ] };
        { line = "y"
        ; expected = [ 1, ["source.multiwhile"] ] }
      ];
    ]
  ]
