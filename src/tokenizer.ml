open Common

type token = { ending : int; scopes : string list }

let ending token = token.ending
let scopes token = token.scopes

type stack_elem = {
  stack_delim : delim;
  stack_enter_pos : int option;
  stack_resume_anchor : int option;
  stack_end_re : regex;
  stack_grammar : grammar;
  stack_repos : (string, repo_item) Hashtbl.t list;
  stack_scopes : string list;
  stack_prev_scopes : string list;
}

type stack = stack_elem list

let empty = []

let rec add_scopes scopes = function
  | [] -> scopes
  | None :: xs -> add_scopes scopes xs
  | Some x :: xs -> add_scopes (x :: scopes) xs

let has_progress start ending = ending > start

type matched_region = {
  region : Oniguruma.Region.t;
  regex : regex;
  end_ : int;
}

type match_result =
  | No_match
  | Empty_match of matched_region
  | Nonempty_match of matched_region

(* Match with \G anchor handling *)
let match_pattern regex line pos anchor =
  let options =
    if anchor <> Some pos then Oniguruma.Options.not_begin_position
    else Oniguruma.Options.none
  in
  match Oniguruma.match_ regex line pos options with
  | None -> No_match
  | Some region ->
    let start = Oniguruma.Region.capture_beg region 0 in
    let end_ = Oniguruma.Region.capture_end region 0 in
    assert (start = pos);
    let matched = { region; regex; end_ } in
    if has_progress start end_ then Nonempty_match matched
    else Empty_match matched

let has_same_delim_at_pos stack delim pos =
  List.exists
    (fun se -> se.stack_enter_pos = Some pos && se.stack_delim == delim)
    stack

(* If the stack is empty, returns the main patterns associated with the
   grammar. Otherwise, returns the patterns associated with the delimiter at
   the top of the stack. *)
let next_pats grammar = function
  | [] -> grammar.patterns
  | s :: _ -> s.stack_delim.delim_patterns

(* Should the character be escaped in a regex? *)
let is_special = function
  | '|' | '.' | '*' | '+' | '?' | '^' | '$' | '-' | ':' | '~' | '#' | '&' | '('
  | ')' | '[' | ']' | '{' | '}' | '<' | '>' | '\\' | '\'' ->
    true
  | _ -> false

(* Insert the substring of [line] from [beg] to [end_] into [buf]. *)
let insert_capture buf line beg end_ =
  let rec loop i =
    if i = end_ then ()
    else
      let ch = line.[i] in
      if is_special ch then Buffer.add_char buf '\\';
      Buffer.add_char buf ch;
      loop (i + 1)
  in
  loop beg

(* Substitute the begin pattern's captures for the backreferences in the end
   delimiter. *)
let subst_backrefs delim line region =
  let { delim_end = regex_str; delim_begin = begin_re; _ } = delim in
  let buf = Buffer.create (String.length regex_str) in
  let num_beg_captures = Oniguruma.num_captures begin_re in
  let regex_len = String.length regex_str in
  let rec loop i escaped =
    if i < regex_len then
      match (regex_str.[i], escaped) with
      | '\\', true ->
        Buffer.add_string buf "\\\\";
        loop (i + 1) false
      | '\\', false -> loop (i + 1) true
      | char, true ->
        if char >= '0' && char <= '9' then (
          let idx = Char.code char - Char.code '0' in
          if idx < num_beg_captures then
            let beg = Oniguruma.Region.capture_beg region idx in
            let end_ = Oniguruma.Region.capture_end region idx in
            if beg <> -1 then insert_capture buf line beg end_)
        else (
          Buffer.add_char buf '\\';
          Buffer.add_char buf char);
        loop (i + 1) false
      | char, false ->
        Buffer.add_char buf char;
        loop (i + 1) false
  in
  loop 0 false;
  Buffer.contents buf

let rec find_nested scope = function
  | [] -> None
  | repo :: repos -> (
    match Hashtbl.find_opt repo scope with
    | Some x -> Some x
    | None -> find_nested scope repos)

(* Discard zero-length tokens. *)
let remove_empties =
  let rec go acc = function
    | [] -> acc
    | tok :: toks ->
      let prev =
        match toks with
        | [] -> 0
        | tok :: _ -> tok.ending
      in
      if tok.ending = prev then go acc toks else go (tok :: acc) toks
  in
  go []

(* Emit tokens for the match region's captures. *)
let handle_captures re scopes default mat_start mat_end region captures tokens
    =
  let captures =
    Array.concat
      (Hashtbl.fold
         (fun k capture acc ->
           let captures =
             match k with
             | Capture_idx idx -> [| (idx, capture) |]
             | Capture_name str ->
               Array.map
                 (fun idx -> (idx, capture))
                 (Oniguruma.name_to_group_numbers re str)
           in
           captures :: acc)
         captures [])
  in
  let captures = Array.to_list captures in
  let captures =
    List.filter_map
      (fun (idx, capture) ->
        if idx < 0 || idx >= Oniguruma.Region.length region then None
        else
          let beg = Oniguruma.Region.capture_beg region idx in
          let end_ = Oniguruma.Region.capture_end region idx in
          Some (capture, beg, end_))
      captures
  in
  let captures =
    List.stable_sort
      (fun (_, a, b) (_, c, d) -> compare (a, b) (d, c))
      captures
  in
  let _, _, stack, tokens =
    (* Do a depth-first traversal by keeping a stack of captures. *)
    List.fold_left
      (fun (prev_idx, start, stack, tokens) (capture, cap_start, cap_end) ->
        (* If the capture mentions a lookahead, it may go past the bounds of
           its parent. Therefore, cap it inside the bounds of the match. *)
        if cap_start = -1 then
          (* Capture wasn't found, ignore *)
          (prev_idx, start, stack, tokens)
        else
          (* Pop while start is greater than or equal to stack top *)
          (* prev_idx to enforce that indices are in increasing order *)
          let rec pop prev_idx start tokens = function
            | [] ->
              let ending = if prev_idx > start then prev_idx else start in
              ( ending,
                { scopes = add_scopes scopes [ default ]; ending } :: tokens,
                [] )
            | (ending, scopes) :: stack' as stack ->
              if start >= ending then
                let ending = if prev_idx > ending then prev_idx else ending in
                pop ending start ({ scopes; ending } :: tokens) stack'
              else
                let ending = if prev_idx > start then prev_idx else start in
                (ending, { scopes; ending } :: tokens, stack)
          in
          let cap_start = if cap_start < start then start else cap_start in
          let cap_end = if cap_end > mat_end then mat_end else cap_end in
          let prev_idx, tokens, stack = pop prev_idx cap_start tokens stack in
          ( prev_idx,
            cap_start,
            (cap_end, add_scopes scopes [ capture.capture_name ]) :: stack,
            tokens ))
      (mat_start, mat_start, [], tokens)
      captures
  in
  let rec pop tokens = function
    | [] -> tokens
    | (ending, scopes) :: stack -> pop ({ scopes; ending } :: tokens) stack
  in
  pop tokens stack

let line_frame grammar stack =
  match stack with
  | [] ->
    ( [ grammar.scope_name ],
      grammar.patterns,
      [ grammar.repository ],
      grammar )
  | se :: _ ->
    ( se.stack_scopes,
      se.stack_delim.delim_patterns,
      se.stack_repos,
      se.stack_grammar )

let emit_scope_token scopes name ending toks =
  { scopes = add_scopes scopes [ name ]; ending } :: toks

let emit_capture_scoped_tokens ~scopes ~name ~captures ~pos matched toks =
  let toks = { scopes; ending = pos } :: toks in
  let toks =
    handle_captures matched.regex scopes name pos matched.end_ matched.region
      captures toks
  in
  emit_scope_token scopes name matched.end_ toks

let emit_delim_end_captures ~prev_scopes ~delim ~pos matched toks =
  let toks =
    {
      scopes =
        add_scopes prev_scopes [ delim.delim_name; delim.delim_content_name ];
      ending = pos;
    }
    :: toks
  in
  handle_captures matched.regex prev_scopes delim.delim_name pos matched.end_
    matched.region delim.delim_end_captures toks

(* Tokenizes a line according to the grammar.

   [t]: The collection of grammars.
   [grammar]: The language grammar.
   [stack]: The stack that keeps track of nested delimiters
   [pos]: The current index into the string.
   [toks]: The list of tokens, with the rightmost ones at the front.
   [line]: The string that is being matched and tokenized.
   [rem_pats]: The remaining patterns yet to be tried *)
let rec match_line ~t ~grammar ~stack ~anchor ~pos ~toks ~line rem_pats =
  let len = String.length line in
  let scopes, stk_pats, repos, cur_grammar = line_frame grammar stack in
  let rec try_pats repos cur_grammar ~k pats =
    let continue_with ~pats () = try_pats repos cur_grammar ~k pats in
    match pats with
    | [] -> k ()
    | Match m :: pats -> (
      match match_pattern m.pattern line pos anchor with
      | No_match | Empty_match _ -> try_pats repos cur_grammar ~k pats
      | Nonempty_match matched ->
        let toks =
          emit_capture_scoped_tokens ~scopes:scopes ~name:m.name
            ~captures:m.captures ~pos matched toks
        in
        match_line ~t ~grammar ~stack ~anchor ~pos:matched.end_ ~toks ~line
          (next_pats grammar stack))
    | Delim d :: pats -> (
      match match_pattern d.delim_begin line pos anchor with
      | No_match -> continue_with ~pats ()
      | Empty_match _ when has_same_delim_at_pos stack d pos ->
        match_line ~t ~grammar ~stack ~anchor ~pos:(pos + 1) ~toks ~line
          (next_pats grammar stack)
      | Empty_match ({ region; end_; _ } as matched)
      | Nonempty_match ({ region; end_; _ } as matched) -> (
        let toks =
          emit_capture_scoped_tokens ~scopes:scopes
            ~name:d.delim_name ~captures:d.delim_begin_captures ~pos matched
            toks
        in
        let child_scopes =
          add_scopes scopes [ d.delim_name; d.delim_content_name ]
        in
        let stack_end_re =
          let pattern = subst_backrefs d line region in
          compile_regex
            ~error_context:("End pattern for " ^ d.delim_end)
            pattern
        in
        let se =
          {
            stack_delim = d;
            stack_enter_pos = Some pos;
            stack_resume_anchor = anchor;
            stack_end_re;
            stack_repos = repos;
            stack_grammar = cur_grammar;
            stack_scopes = child_scopes;
            stack_prev_scopes = scopes;
          }
        in
        match d.delim_kind with
        | End ->
          match_line ~t ~grammar ~stack:(se :: stack) ~anchor:(Some end_)
            ~pos:end_ ~toks ~line d.delim_patterns
        | While ->
          ( remove_empties ({ scopes = child_scopes; ending = len } :: toks),
            se :: stack )))
    | Scope_patterns { scope_name = _; child_patterns } :: pats ->
      try_pats repos cur_grammar child_patterns ~k:(continue_with ~pats)
    | Include_scope name :: pats -> (
      match find_by_scope_name t name with
      | None -> try_pats repos cur_grammar ~k pats
      | Some nested_grammar ->
        try_pats
          [ nested_grammar.repository ]
          nested_grammar nested_grammar.patterns ~k:(continue_with ~pats))
    | Include_base :: pats ->
      try_pats [ grammar.repository ] grammar grammar.patterns
        ~k:(continue_with ~pats)
    | Include_self :: pats ->
      try_pats [ cur_grammar.repository ] cur_grammar cur_grammar.patterns
        ~k:(continue_with ~pats)
    | Include_local key :: pats -> (
      match find_nested key repos with
      | None -> try_pats repos cur_grammar ~k pats
      | Some item -> (
        match item.repo_item_kind with
        | Repo_rule rule ->
          try_pats (item.repo_inner :: repos) cur_grammar (rule :: pats) ~k
        | Repo_patterns pats' ->
          try_pats (item.repo_inner :: repos) cur_grammar pats'
            ~k:(continue_with ~pats)))
  in
  let try_delim_end stack_top stack_tail ~k =
    let delim = stack_top.stack_delim in
    let end_match = match_pattern stack_top.stack_end_re line pos anchor in
    let pop_after_close matched toks =
      let toks =
        emit_scope_token scopes delim.delim_name matched.end_ toks
      in
      match_line ~t ~grammar ~stack:stack_tail
        ~anchor:stack_top.stack_resume_anchor ~pos:matched.end_ ~toks ~line
        (next_pats grammar stack_tail)
    in
    match (delim.delim_kind, end_match) with
    | End, No_match -> k ()
    | End, Empty_match matched when stack_top.stack_enter_pos = Some pos ->
      let toks =
        emit_delim_end_captures ~prev_scopes:stack_top.stack_prev_scopes ~delim
          ~pos matched toks
      in
      match_line ~t ~grammar ~stack:stack_tail
        ~anchor:stack_top.stack_resume_anchor ~pos:(pos + 1) ~toks ~line
        (next_pats grammar stack_tail)
    | End, (Empty_match matched | Nonempty_match matched) ->
      let toks =
        emit_delim_end_captures ~prev_scopes:stack_top.stack_prev_scopes ~delim
          ~pos matched toks
      in
      pop_after_close matched toks
    | While, _ -> error "Unreachable"
  in
  if pos > len then
    match stack with
    | [] ->
      ( remove_empties ({ scopes; ending = len } :: toks),
        stack )
    | se :: _ ->
      let end_scopes = add_scopes scopes [ se.stack_delim.delim_name ] in
      (remove_empties ({ scopes = end_scopes; ending = len } :: toks), stack)
  else
    let continue_without_match () =
      match_line ~t ~grammar:cur_grammar ~stack ~anchor ~pos:(pos + 1)
        ~toks ~line stk_pats
    in
    match stack with
    | [] -> try_pats repos grammar rem_pats ~k:continue_without_match
    | se :: stack' -> (
      match se.stack_delim.delim_kind with
      | While ->
        try_pats repos se.stack_grammar rem_pats
          ~k:continue_without_match
      | End ->
        if se.stack_delim.delim_apply_end_pattern_last then
          try_pats repos se.stack_grammar rem_pats ~k:(fun () ->
              try_delim_end se stack' ~k:continue_without_match)
        else
          try_delim_end se stack' ~k:(fun () ->
              try_pats repos se.stack_grammar rem_pats
                ~k:continue_without_match))

let tokenize_exn t grammar stack line =
  (* See https://github.com/Microsoft/vscode-textmate/issues/25 for how to
     handle while rules. This is important for the Markdown grammar. *)
  let rec try_while_rules pos anchor toks rem_stack = function
    | [] -> (toks, pos, anchor, rem_stack)
    | se :: stack -> (
      match se.stack_delim.delim_kind with
      | End -> try_while_rules pos anchor toks (se :: rem_stack) stack
      | While ->
        let rec loop pos' =
          if pos' = String.length line then (toks, pos, anchor, rem_stack)
          else
            match match_pattern se.stack_end_re line pos' anchor with
            | No_match | Empty_match _ -> loop (pos' + 1)
            | Nonempty_match matched ->
              let toks =
                emit_capture_scoped_tokens ~scopes:se.stack_prev_scopes
                  ~name:se.stack_delim.delim_name
                  ~captures:se.stack_delim.delim_end_captures ~pos:pos' matched
                  toks
              in
              try_while_rules matched.end_ (Some matched.end_) toks
                (se :: rem_stack) stack
        in
        loop pos)
  in
  let toks, pos, anchor, stack =
    try_while_rules 0 None [] [] (List.rev stack)
  in
  let toks, stack =
    match_line ~t ~grammar ~stack ~anchor ~pos ~toks ~line
      (next_pats grammar stack)
  in
  (* Reset per-line state before returning the stack for the next line *)
  let stack =
    List.map
      (fun se ->
        { se with stack_enter_pos = None; stack_resume_anchor = None })
      stack
  in
  (toks, stack)
