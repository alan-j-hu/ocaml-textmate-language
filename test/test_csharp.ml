open Util

let () =
  Alcotest.run "C#"
    [
      test_tokenize_plist "data/csharp.tmLanguage" "source.cs"
        [
          [
            {
              line =
                "using StreamReader streamReader = \
                 new(Console.OpenStandardInput());";
              expected =
                [
                  (5, [ "keyword.other.directive.using.cs"; "source.cs" ]);
                  (6, [ "source.cs" ]);
                  (18, [ "entity.name.type.namespace.cs"; "source.cs" ]);
                  (19, [ "source.cs" ]);
                  (31, [ "entity.name.type.namespace.cs"; "source.cs" ]);
                  (32, [ "source.cs" ]);
                  (33, [ "keyword.operator.assignment.cs"; "source.cs" ]);
                  (34, [ "source.cs" ]);
                  (37, [ "entity.name.type.namespace.cs"; "source.cs" ]);
                  (38, [ "source.cs" ]);
                  (45, [ "entity.name.type.namespace.cs"; "source.cs" ]);
                  (46, [ "punctuation.accessor.cs"; "source.cs" ]);
                  (63, [ "entity.name.type.namespace.cs"; "source.cs" ]);
                  (66, [ "source.cs" ]);
                  (67, [ "punctuation.terminator.statement.cs"; "source.cs" ]);
                ];
            };
          ];
        ];
    ]
