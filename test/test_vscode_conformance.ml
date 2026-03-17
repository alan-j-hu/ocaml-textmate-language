type vscode_token = { value : string; scopes : string list }
type vscode_line = { line : string; tokens : vscode_token list }

type vscode_case = {
  desc : string;
  grammars : string list;
  grammar_path : string option;
  grammar_scope_name : string option;
  grammar_injections : string list;
  lines : vscode_line list;
}

type grammar_info = {
  rel_path : string;
  full_path : string;
  scope_name : string;
  grammar : TmLanguage.grammar;
}

let assoc_opt key = function
  | `Assoc pairs -> List.assoc_opt key pairs
  | _ -> None

let as_string = function
  | `String s -> s
  | _ -> failwith "Expected JSON string"

let as_list = function
  | `List xs -> xs
  | _ -> failwith "Expected JSON list"

let required key conv missing_msg json =
  match assoc_opt key json with
  | Some v -> conv v
  | None -> failwith missing_msg

let parse_token json =
  let value = required "value" as_string "Missing token.value" json in
  let scopes =
    required "scopes"
      (fun v -> List.map as_string (as_list v))
      "Missing token.scopes" json
  in
  { value; scopes }

let parse_line json =
  let line = required "line" as_string "Missing line.line" json in
  let tokens =
    required "tokens"
      (fun v -> List.map parse_token (as_list v))
      "Missing line.tokens" json
  in
  { line; tokens }

let parse_case json =
  let desc =
    match assoc_opt "desc" json with
    | Some v -> as_string v
    | None -> "unnamed-vscode-case"
  in
  let grammars =
    match assoc_opt "grammars" json with
    | Some v -> List.map as_string (as_list v)
    | None -> failwith "Missing case.grammars"
  in
  let grammar_path = Option.map as_string (assoc_opt "grammarPath" json) in
  let grammar_scope_name =
    Option.map as_string (assoc_opt "grammarScopeName" json)
  in
  let grammar_injections =
    match assoc_opt "grammarInjections" json with
    | None -> []
    | Some v -> List.map as_string (as_list v)
  in
  let lines =
    match assoc_opt "lines" json with
    | Some v -> List.map parse_line (as_list v)
    | None -> failwith "Missing case.lines"
  in
  {
    desc;
    grammars;
    grammar_path;
    grammar_scope_name;
    grammar_injections;
    lines;
  }

let parse_cases path =
  match Yojson.Basic.from_file path with
  | `List xs -> List.map parse_case xs
  | _ -> failwith "Expected top-level JSON list"

let with_open_in path f =
  let chan = open_in path in
  Fun.protect (fun () -> f chan) ~finally:(fun () -> close_in chan)

let scope_name_of_json json path =
  match json with
  | `Assoc pairs -> (
    match List.assoc_opt "scopeName" pairs with
    | Some (`String s) -> s
    | _ -> failwith ("Grammar missing scopeName: " ^ path))
  | _ -> failwith ("Invalid grammar JSON: " ^ path)

let scope_name_of_plist plist path =
  match plist with
  | `Dict pairs -> (
    match List.assoc_opt "scopeName" pairs with
    | Some (`String s) -> s
    | _ -> failwith ("Grammar missing scopeName: " ^ path))
  | _ -> failwith ("Invalid grammar plist: " ^ path)

let load_grammar_info rel_path full_path =
  if Filename.extension full_path = ".json" then
    let raw = Yojson.Basic.from_file full_path in
    {
      rel_path;
      full_path;
      scope_name = scope_name_of_json raw full_path;
      grammar = TmLanguage.of_yojson_exn raw;
    }
  else
    with_open_in full_path (fun chan ->
        let raw = Plist_xml.from_channel chan in
        {
          rel_path;
          full_path;
          scope_name = scope_name_of_plist raw full_path;
          grammar = TmLanguage.of_plist_exn raw;
        })

let expected_line_tokens line expected =
  let expected =
    if String.length line = 0 then expected
    else List.filter (fun tok -> tok.value <> "") expected
  in
  let _, rev =
    List.fold_left
      (fun (pos, acc) tok ->
        let next_pos = pos + String.length tok.value in
        (next_pos, (next_pos, List.rev tok.scopes) :: acc))
      (0, []) expected
  in
  List.rev rev

let run_case tests_path case () =
  let root = Filename.dirname tests_path in
  let infos =
    List.map
      (fun rel_path ->
        let full_path = Filename.concat root rel_path in
        load_grammar_info rel_path full_path)
      case.grammars
  in
  let collection = TmLanguage.create () in
  List.iter (fun info -> TmLanguage.add_grammar collection info.grammar) infos;
  let target_scope_name =
    match (case.grammar_scope_name, case.grammar_path) with
    | Some scope_name, _ -> scope_name
    | None, Some grammar_path ->
      let rec find_scope = function
        | [] -> failwith ("Target grammar file not loaded: " ^ grammar_path)
        | info :: rest ->
          if info.rel_path = grammar_path then info.scope_name
          else find_scope rest
      in
      find_scope infos
    | None, None -> failwith "Case is missing grammarScopeName and grammarPath"
  in
  let grammar =
    match TmLanguage.find_by_scope_name collection target_scope_name with
    | Some grammar -> grammar
    | None -> failwith ("Target grammar not loaded: " ^ target_scope_name)
  in
  let tested_type = Alcotest.(list (pair int (list string))) in
  ignore
    (List.fold_left
       (fun stack line_case ->
         let actual, stack =
           TmLanguage.tokenize_exn collection grammar stack line_case.line
         in
         let actual =
           List.map
             (fun tok -> (TmLanguage.ending tok, TmLanguage.scopes tok))
             actual
         in
         let expected = expected_line_tokens line_case.line line_case.tokens in
         Alcotest.check tested_type
           (case.desc ^ " :: " ^ line_case.line)
           expected actual;
         stack)
       TmLanguage.empty case.lines)

let case_pending_reason tests_path case =
  let root = Filename.dirname tests_path in
  if case.grammar_injections <> [] then Some "grammar-injections"
  else
    let full_paths =
      List.map (fun rel -> Filename.concat root rel) case.grammars
    in
    let rec first_missing = function
      | [] -> None
      | path :: rest ->
        if Sys.file_exists path then first_missing rest else Some path
    in
    match first_missing full_paths with
    | Some path -> Some ("missing-fixture:" ^ path)
    | None -> None

let resolve_path_opt candidates = List.find_opt Sys.file_exists candidates

let suite_specs =
  [
    ( "phase5/tests",
      [ "vscode/phase5/tests.json"; "test/vscode/phase5/tests.json" ] );
    ( "phase4/tests",
      [ "vscode/phase4/tests.json"; "test/vscode/phase4/tests.json" ] );
    ( "suite1/whileTests",
      [ "vscode/suite1/whileTests.json"; "test/vscode/suite1/whileTests.json" ]
    );
    ( "suite1/tests",
      [ "vscode/suite1/tests.json"; "test/vscode/suite1/tests.json" ] );
    ( "first-mate/tests",
      [ "vscode/first-mate/tests.json"; "test/vscode/first-mate/tests.json" ]
    );
  ]

let loaded_suites =
  let rec load acc = function
    | [] -> List.rev acc
    | (name, candidates) :: rest -> (
      match resolve_path_opt candidates with
      | None -> load acc rest
      | Some path -> load ((name, path, parse_cases path) :: acc) rest)
  in
  load [] suite_specs

let make_case_test tests_path case =
  match case_pending_reason tests_path case with
  | None -> Alcotest.test_case case.desc `Quick (run_case tests_path case)
  | Some reason ->
    Alcotest.test_case
      (case.desc ^ " [pending:" ^ reason ^ "]")
      `Quick
      (fun () -> ())

let import_tests =
  List.map
    (fun (name, path, cases) ->
      Alcotest.test_case ("loads " ^ name) `Quick (fun () ->
          Alcotest.(check bool) ("exists " ^ path) true (Sys.file_exists path);
          Alcotest.(check bool)
            ("non-empty " ^ name) true
            (List.length cases > 0)))
    loaded_suites

let suite_groups =
  List.map
    (fun (name, path, cases) -> (name, List.map (make_case_test path) cases))
    loaded_suites

let () =
  Alcotest.run "VSCode TextMate Conformance"
    (("import", import_tests) :: suite_groups)
