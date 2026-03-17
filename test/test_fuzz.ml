open Util

type fuzz_target = {
  name : string;
  scope : string;
  grammar_paths : string list;
  iterations : int;
  allow_malformed_end_error : bool;
}

let load_grammar path =
  if Filename.extension path = ".json" then read_yojson_basic path
  else read_plist path

let random_alphabet =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 \
   (){}[]#\"'_-+*/=:."

let malformed_end_prefix = "End pattern:"

let random_line rng =
  let len = Random.State.int rng 48 in
  String.init len (fun _ ->
      random_alphabet.[Random.State.int rng (String.length random_alphabet)])

let assert_token_invariants line toks =
  let len = String.length line in
  match toks with
  | [] -> Alcotest.fail "tokenize_exn returned an empty token list"
  | first :: rest ->
    let first_end = TmLanguage.ending first in
    if first_end < 0 || first_end > len then
      Alcotest.failf "token ending out of bounds: %d (line length %d)"
        first_end len;
    let last_end =
      List.fold_left
        (fun prev tok ->
          let ending = TmLanguage.ending tok in
          if ending <= prev then
            Alcotest.failf "non-increasing endings: prev=%d current=%d" prev
              ending;
          if ending < 0 || ending > len then
            Alcotest.failf "token ending out of bounds: %d (line length %d)"
              ending len;
          ending)
        first_end rest
    in
    if last_end <> len then
      Alcotest.failf "last token ending %d differs from line length %d"
        last_end len

let run_fuzz_case target () =
  let t = TmLanguage.create () in
  List.iter
    (fun path -> TmLanguage.add_grammar t (load_grammar path))
    target.grammar_paths;
  let grammar =
    match TmLanguage.find_by_scope_name t target.scope with
    | Some g -> g
    | None -> Alcotest.failf "scope not found in grammar set: %s" target.scope
  in
  let rng = Random.State.make [| 0x5EED; String.length target.name |] in
  let rec loop n stack =
    if n = target.iterations then ()
    else
      let line = random_line rng in
      match TmLanguage.tokenize_exn t grammar stack line with
      | toks, stack ->
        assert_token_invariants line toks;
        loop (n + 1) stack
      | exception TmLanguage.Error msg ->
        if
          target.allow_malformed_end_error
          && String.length msg >= String.length malformed_end_prefix
          && String.sub msg 0 (String.length malformed_end_prefix)
             = malformed_end_prefix
        then loop (n + 1) TmLanguage.empty
        else Alcotest.failf "tokenize_exn raised Error on line %S: %s" line msg
  in
  loop 0 TmLanguage.empty

let targets =
  [
    {
      name = "a-json";
      scope = "source.a";
      grammar_paths = [ "data/a.json" ];
      iterations = 800;
      allow_malformed_end_error = false;
    };
    {
      name = "while-json";
      scope = "source.while";
      grammar_paths = [ "data/while.json" ];
      iterations = 800;
      allow_malformed_end_error = false;
    };
    {
      name = "apply-end-pattern-last";
      scope = "source.apply-end-pattern-last";
      grammar_paths = [ "vscode/phase4/fixtures/apply-end-pattern-last.json" ];
      iterations = 600;
      allow_malformed_end_error = false;
    };
    {
      name = "backref-escaping";
      scope = "source.backreference-escaping";
      grammar_paths = [ "vscode/phase4/fixtures/239.tmLanguage.json" ];
      iterations = 600;
      allow_malformed_end_error = true;
    };
    {
      name = "scope-repo-include";
      scope = "test.include-external-repository-rule";
      grammar_paths =
        [
          "vscode/phase5/fixtures/include-external-repository-rule.json";
          "vscode/phase5/fixtures/python-mini.json";
        ];
      iterations = 600;
      allow_malformed_end_error = false;
    };
  ]

let () =
  let tests =
    List.map
      (fun target ->
        Alcotest.test_case target.name `Quick (run_fuzz_case target))
      targets
  in
  Alcotest.run "Tokenizer Fuzz" [ ("fuzz", tests) ]
