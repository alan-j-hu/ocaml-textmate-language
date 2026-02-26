open Util

let () =
  Alcotest.run "OCaml"
    [
      test_tokenize_json "data/ocaml.tmLanguage.json" "source.ocaml"
        [
          [
            {
              line = "let x = 1";
              expected =
                [
                  (3, [ "keyword.ocaml"; "source.ocaml" ]);
                  (4, [ "source.ocaml" ]);
                  (5, [ "entity.name.function.binding.ocaml"; "source.ocaml" ]);
                  (6, [ "source.ocaml" ]);
                  (7, [ "keyword.operator.ocaml"; "source.ocaml" ]);
                  (8, [ "source.ocaml" ]);
                  (9, [ "constant.numeric.decimal.integer.ocaml"; "source.ocaml" ]);
                ];
            };
          ];
        ];
    ]
