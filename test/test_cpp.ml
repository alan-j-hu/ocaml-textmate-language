open Util

let () =
  Alcotest.run "C++"
    [
      test_tokenize "data/cpp.tmLanguage.json" "source.cpp"
        [
          [
            {
              line = "int main()";
              expected =
                [
                  (3, [ "meta.qualified_type.cpp"; "source.cpp" ]);
                  (4, [ "source.cpp" ]);
                  (8, [ "entity.name.function.definition.cpp"; "source.cpp" ]);
                  ( 9,
                    [
                      "punctuation.section.parameters.begin.bracket.round.cpp";
                      "meta.head.function.definition.cpp";
                      "meta.function.definition.cpp";
                      "source.cpp";
                    ] );
                  ( 10,
                    [
                      "punctuation.section.parameters.end.bracket.round.cpp";
                      "meta.head.function.definition.cpp";
                      "meta.function.definition.cpp";
                      "source.cpp";
                    ] );
                ];
            };
          ];
        ];
    ]
