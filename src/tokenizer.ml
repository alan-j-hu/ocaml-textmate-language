open Common

type token = { ending : int; scopes : string list }

let ending token = token.ending
let scopes token = token.scopes

type stack_elem = {
  stack_delim : delim;
  stack_enter_pos : int;
  stack_end_anchor_pos : int;
  stack_end_re : regex;
  stack_end_re_parent_anchor : regex option;
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

type matched_region = { region : Oniguruma.Region.t; end_ : int }

type match_result =
  | No_match
  | Empty_match of matched_region
  | Nonempty_match of matched_region

let match_regex regex line pos options =
  match Oniguruma.match_ regex line pos options with
  | None -> No_match
  | Some region ->
    let start = Oniguruma.Region.capture_beg region 0 in
    let end_ = Oniguruma.Region.capture_end region 0 in
    assert (start = pos);
    let matched = { region; end_ } in
    if has_progress pos end_ then Nonempty_match matched
    else Empty_match matched

let has_same_delim_at_pos stack delim pos =
  List.exists
    (fun se -> se.stack_enter_pos = pos && se.stack_delim == delim)
    stack

let split_scope_repo_include s =
  match String.index_opt s '#' with
  | None -> None
  | Some i ->
    if i = 0 || i = String.length s - 1 then None
    else
      let scope = String.sub s 0 i in
      let key = String.sub s (i + 1) (String.length s - i - 1) in
      Some (scope, key)

(* If the stack is empty, returns the main patterns associated with the
   grammar. Otherwise, returns the patterns associated with the delimiter at
   the top of the stack. *)
let next_pats grammar = function
  | [] -> grammar.patterns
  | s :: _ -> s.stack_delim.delim_patterns

let is_digit ch = ch >= '0' && ch <= '9'

let is_literal_char ch =
  match ch with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

(* Insert the substring of [line] from [beg] to [end_] into [buf]. *)
let insert_capture buf line beg end_ =
  let rec loop i =
    if i = end_ then ()
    else
      let ch = line.[i] in
      if not (is_literal_char ch) then Buffer.add_char buf '\\';
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
        if is_digit char then (
          let j = ref i in
          while !j < regex_len && is_digit regex_str.[!j] do
            incr j
          done;
          let idx = int_of_string (String.sub regex_str i (!j - i)) in
          if idx <= num_beg_captures then (
            let beg = Oniguruma.Region.capture_beg region idx in
            let end_ = Oniguruma.Region.capture_end region idx in
            if beg <> -1 then insert_capture buf line beg end_)
          else (
            Buffer.add_char buf '\\';
            Buffer.add_substring buf regex_str i (!j - i));
          loop !j false)
        else (
          Buffer.add_char buf '\\';
          Buffer.add_char buf char;
          loop (i + 1) false)
      | char, false ->
        Buffer.add_char buf char;
        loop (i + 1) false
  in
  loop 0 false;
  Buffer.contents buf

let rewrite_g_anchor_to_absolute_opt regex =
  let len = String.length regex in
  let buf = Buffer.create len in
  let rec loop i in_char_class saw_g_anchor =
    if i >= len then if saw_g_anchor then Some (Buffer.contents buf) else None
    else
      let ch = regex.[i] in
      if ch = '\\' then
        if i + 1 >= len then (
          Buffer.add_char buf '\\';
          if saw_g_anchor then Some (Buffer.contents buf) else None)
        else
          let next = regex.[i + 1] in
          if (not in_char_class) && next = 'G' then (
            Buffer.add_string buf "\\A";
            loop (i + 2) in_char_class true)
          else (
            Buffer.add_char buf '\\';
            Buffer.add_char buf next;
            loop (i + 2) in_char_class saw_g_anchor)
      else
        let in_char_class =
          if ch = '[' then true else if ch = ']' then false else in_char_class
        in
        Buffer.add_char buf ch;
        loop (i + 1) in_char_class saw_g_anchor
  in
  loop 0 false false

let compile_regex pattern delim =
  match
    Oniguruma.create pattern Oniguruma.Options.none Oniguruma.Encoding.utf8
      Oniguruma.Syntax.default
  with
  | Error e -> error ("End pattern: " ^ delim.delim_end ^ ": " ^ e)
  | Ok re -> re

let select_end_match_context ~line ~anchor_pos ~default_re ~parent_anchor_re =
  match parent_anchor_re with
  | None -> (default_re, 0, line)
  | Some re ->
    if anchor_pos >= String.length line then (default_re, 0, line)
    else
      let line_for_match =
        String.sub line anchor_pos (String.length line - anchor_pos)
      in
      (re, anchor_pos, line_for_match)

let match_end_at ~re ~offset ~line_for_match ~pos =
  let pos_for_match = pos - offset in
  if pos_for_match < 0 then None
  else
    match
      Oniguruma.match_ re line_for_match pos_for_match Oniguruma.Options.none
    with
    | None -> None
    | Some region ->
      let start = Oniguruma.Region.capture_beg region 0 + offset in
      let end_ = Oniguruma.Region.capture_end region 0 + offset in
      Some (region, start, end_)

let match_subst_for delim line region =
  let pattern = subst_backrefs delim line region in
  let re = compile_regex pattern delim in
  let re_parent_anchor =
    match rewrite_g_anchor_to_absolute_opt pattern with
    | Some rewritten -> Some (compile_regex rewritten delim)
    | None -> None
  in
  (re, re_parent_anchor)

let rec find_nested scope = function
  | [] -> None
  | repo :: repos -> (
    match Hashtbl.find_opt repo scope with
    | Some x -> Some x
    | None -> find_nested scope repos)

let emit_delim_close_token ~base_scopes ~delim_name ~ending toks =
  { scopes = add_scopes base_scopes [ delim_name ]; ending } :: toks

(* Discard zero-length tokens. *)
let remove_empties =
  let rec go acc = function
    | [] -> acc
    | [ tok ] -> if tok.ending = 0 && acc <> [] then acc else tok :: acc
    | tok :: toks ->
      let prev =
        match toks with
        | [] -> assert false
        | tok :: _ -> tok.ending
      in
      if tok.ending = prev then go acc toks else go (tok :: acc) toks
  in
  go []

let resolve_capture_name ~line ~region ~region_offset name =
  let len = String.length name in
  let buf = Buffer.create len in
  let rec loop i =
    if i >= len then ()
    else if name.[i] = '$' then (
      let j = ref (i + 1) in
      while !j < len && name.[!j] >= '0' && name.[!j] <= '9' do
        incr j
      done;
      if !j = i + 1 then (
        Buffer.add_char buf '$';
        loop (i + 1))
      else
        let idx = int_of_string (String.sub name (i + 1) (!j - i - 1)) in
        if idx >= 0 && idx < Oniguruma.Region.length region then (
          let beg = Oniguruma.Region.capture_beg region idx + region_offset in
          let end_ = Oniguruma.Region.capture_end region idx + region_offset in
          if beg >= 0 && end_ >= beg && end_ <= String.length line then
            Buffer.add_substring buf line beg (end_ - beg);
          loop !j)
        else (
          Buffer.add_char buf name.[i];
          loop (i + 1)))
    else (
      Buffer.add_char buf name.[i];
      loop (i + 1))
  in
  loop 0;
  Buffer.contents buf

let merge_repos repos =
  let merged = Hashtbl.create 31 in
  List.iter
    (fun repo -> Hashtbl.iter (fun k v -> Hashtbl.replace merged k v) repo)
    (List.rev repos);
  merged

(* Emit tokens for the match region's captures. *)
let handle_captures ?(region_offset = 0) ?tokenize_capture_patterns ~line re
    scopes default mat_start mat_end region captures tokens =
  let region_len = Oniguruma.Region.length region in
  let captures =
    Hashtbl.fold
      (fun k capture acc ->
        match k with
        | Capture_idx idx -> (idx, capture) :: acc
        | Capture_name str ->
          let groups = Oniguruma.name_to_group_numbers re str in
          Array.fold_right (fun idx acc -> (idx, capture) :: acc) groups acc)
      captures []
  in
  let captures =
    List.filter_map
      (fun (idx, capture) ->
        if idx < 0 || idx >= region_len then None
        else
          let beg = Oniguruma.Region.capture_beg region idx in
          let end_ = Oniguruma.Region.capture_end region idx in
          Some (idx, capture, beg + region_offset, end_ + region_offset))
      captures
  in
  let captures =
    List.stable_sort
      (fun (idx1, _, start1, end1) (idx2, _, start2, end2) ->
        let by_start = compare start1 start2 in
        if by_start <> 0 then by_start
        else
          let by_end = compare end2 end1 in
          if by_end <> 0 then by_end else compare idx1 idx2)
      captures
  in
  let emit_segment prev_idx tokens start_pos end_pos seg_scopes seg_patterns =
    let end_pos = max prev_idx end_pos in
    let start_pos = max prev_idx start_pos in
    if start_pos >= end_pos then (end_pos, tokens)
    else
      match (seg_patterns, tokenize_capture_patterns) with
      | [], _ | _, None ->
        (end_pos, { scopes = seg_scopes; ending = end_pos } :: tokens)
      | patterns, Some tokenize ->
        let nested = tokenize seg_scopes patterns start_pos end_pos in
        let tokens =
          List.fold_left (fun acc tok -> tok :: acc) tokens nested
        in
        (end_pos, tokens)
  in
  let prev_idx, _, stack, tokens =
    (* Do a depth-first traversal by keeping a stack of captures. *)
    List.fold_left
      (fun (prev_idx, start, stack, tokens) (_, capture, cap_start, cap_end) ->
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
              let ending = max prev_idx start in
              ( ending,
                { scopes = add_scopes scopes [ default ]; ending } :: tokens,
                [] )
            | (start_pos, ending, scopes, patterns) :: stack' as stack ->
              if start >= ending then
                let prev_idx, tokens =
                  emit_segment prev_idx tokens start_pos ending scopes patterns
                in
                pop prev_idx start tokens stack'
              else
                let ending = max prev_idx start in
                (ending, { scopes; ending } :: tokens, stack)
          in
          let cap_start = max cap_start start in
          let cap_end = min cap_end mat_end in
          let prev_idx, tokens, stack = pop prev_idx cap_start tokens stack in
          let capture_name =
            match capture.capture_name with
            | None -> None
            | Some name when String.contains name '$' ->
              Some (resolve_capture_name ~line ~region ~region_offset name)
            | Some name -> Some name
          in
          let capture_scopes = add_scopes scopes [ default; capture_name ] in
          ( prev_idx,
            cap_start,
            (cap_start, cap_end, capture_scopes, capture.capture_patterns)
            :: stack,
            tokens ))
      (mat_start, mat_start, [], tokens)
      captures
  in
  let rec pop prev_idx tokens = function
    | [] -> tokens
    | (start_pos, end_pos, scopes, patterns) :: stack ->
      let prev_idx, tokens =
        emit_segment prev_idx tokens start_pos end_pos scopes patterns
      in
      pop prev_idx tokens stack
  in
  pop prev_idx tokens stack

let emit_end_captures ~line ~re ~offset ~tokenize_capture_patterns
    ~capture_scopes ~prefix_scopes ~delim_name ~delim_end_captures ~pos ~end_
    ~region toks =
  let toks = { scopes = prefix_scopes; ending = pos } :: toks in
  handle_captures ~region_offset:offset ~line re ~tokenize_capture_patterns
    capture_scopes delim_name pos end_ region delim_end_captures toks

let emit_begin_match_tokens ~line ~re ~tokenize_capture_patterns ~scopes ~name
    ~captures ~pos ~end_ ~region toks =
  let toks = { scopes; ending = pos } :: toks in
  let toks =
    handle_captures ~line ~tokenize_capture_patterns re scopes name pos end_
      region captures toks
  in
  { scopes = add_scopes scopes [ name ]; ending = end_ } :: toks

(* Tokenizes a line according to the grammar.

   [t]: The collection of grammars.
   [grammar]: The language grammar.
   [stack]: The stack that keeps track of nested delimiters
   [pos]: The current index into the string.
   [toks]: The list of tokens, with the rightmost ones at the front.
   [line]: The string that is being matched and tokenized.
   [rem_pats]: The remaining patterns yet to be tried *)
let rec match_line ~t ~grammar ~stack ~pos ~toks ~line rem_pats =
  let len = String.length line in
  let scopes, stk_pats, repos, cur_grammar =
    match stack with
    | [] ->
      ( [ grammar.scope_name ],
        grammar.patterns,
        [ grammar.repository ],
        grammar )
    | se :: _ ->
      let d = se.stack_delim in
      (se.stack_scopes, d.delim_patterns, se.stack_repos, se.stack_grammar)
  in
  let run_capture_patterns base_scopes patterns start end_ =
    tokenize_capture_patterns ~t ~repos ~line ~base_scopes ~start ~end_
      patterns
  in
  (* Try each pattern in the list until one matches. If none match, increment
     [pos] and try all the patterns again. *)
  let rec try_pats repos cur_grammar ~k pats =
    let continue pats () = try_pats repos cur_grammar ~k pats in
    let try_nested nested_repos nested_grammar nested_patterns pats =
      try_pats nested_repos nested_grammar ~k:(continue pats) nested_patterns
    in
    match pats with
    | [] -> k () (* No patterns have matched, so call the continuation *)
    | Match m :: pats -> (
      match match_regex m.pattern line pos Oniguruma.Options.none with
      | No_match | Empty_match _ -> continue pats ()
      | Nonempty_match { region; end_ } ->
        let toks =
          emit_begin_match_tokens ~line ~re:m.pattern
            ~tokenize_capture_patterns:run_capture_patterns ~scopes
            ~name:m.name ~captures:m.captures ~pos ~end_ ~region toks
        in
        match_line ~t ~grammar ~stack ~pos:end_ ~toks ~line
          (next_pats grammar stack))
    | Delim d :: pats -> (
      (* Try to match the delimiter's begin pattern *)
      match match_regex d.delim_begin line pos Oniguruma.Options.none with
      | No_match -> continue pats ()
      | Empty_match _ when has_same_delim_at_pos stack d pos ->
        match_line ~t ~grammar ~stack ~pos:(pos + 1) ~toks ~line
          (next_pats grammar stack)
      | Empty_match { region; end_ } | Nonempty_match { region; end_ } -> (
        let toks =
          emit_begin_match_tokens ~line ~re:d.delim_begin
            ~tokenize_capture_patterns:run_capture_patterns ~scopes
            ~name:d.delim_name ~captures:d.delim_begin_captures ~pos ~end_
            ~region toks
        in
        let se =
          let stack_end_re, stack_end_re_parent_anchor =
            match_subst_for d line region
          in
          {
            stack_delim = d;
            stack_enter_pos = pos;
            stack_end_anchor_pos = end_;
            stack_end_re;
            stack_end_re_parent_anchor;
            stack_repos = repos;
            stack_grammar = cur_grammar;
            stack_scopes =
              add_scopes scopes [ d.delim_name; d.delim_content_name ];
            stack_prev_scopes = scopes;
          }
        in
        match d.delim_kind with
        | End ->
          (* Push the delimiter on the stack and continue. *)
          match_line ~t ~grammar ~stack:(se :: stack) ~pos:end_ ~toks ~line
            d.delim_patterns
        | While ->
          (* Subsume the remainder of the line into a span. *)
          ( remove_empties
              ({
                 scopes =
                   add_scopes scopes [ d.delim_name; d.delim_content_name ];
                 ending = len;
               }
              :: toks),
            se :: stack )))
    | Scope_patterns { scope_name = _; child_patterns } :: pats ->
      (* Expand child patterns inline with fallback continuation *)
      try_pats repos cur_grammar child_patterns ~k:(continue pats)
    | Include_scope name :: pats -> (
      match split_scope_repo_include name with
      | None -> (
        match find_by_scope_name t name with
        | None ->
          (* Grammar not found; try the next pattern. *)
          continue pats ()
        | Some nested_grammar ->
          try_nested
            [ nested_grammar.repository ]
            nested_grammar nested_grammar.patterns pats)
      | Some (scope_name, key) -> (
        match find_by_scope_name t scope_name with
        | None -> continue pats ()
        | Some nested_grammar -> (
          match Hashtbl.find_opt nested_grammar.repository key with
          | None -> continue pats ()
          | Some item -> (
            let nested_repos =
              item.repo_inner :: [ nested_grammar.repository ]
            in
            match item.repo_item_kind with
            | Repo_rule rule ->
              try_nested nested_repos nested_grammar [ rule ] pats
            | Repo_patterns pats' ->
              try_nested nested_repos nested_grammar pats' pats))))
    | Include_base :: pats ->
      try_nested [ grammar.repository ] grammar grammar.patterns pats
    | Include_self :: pats ->
      try_nested [ cur_grammar.repository ] cur_grammar cur_grammar.patterns
        pats
    | Include_local key :: pats -> (
      match find_nested key repos with
      | None -> continue pats ()
      | Some item -> (
        match item.repo_item_kind with
        | Repo_rule rule ->
          try_pats (item.repo_inner :: repos) cur_grammar (rule :: pats) ~k
        | Repo_patterns pats' ->
          try_nested (item.repo_inner :: repos) cur_grammar pats' pats))
  in
  let try_delim stack_top stack' ~k =
    (* Try to match the delimiter's end pattern *)
    let delim = stack_top.stack_delim in
    let run_capture_patterns base_scopes patterns start end_ =
      tokenize_capture_patterns ~t ~repos:stack_top.stack_repos ~line
        ~base_scopes ~start ~end_ patterns
    in
    let re, offset, line_for_match =
      select_end_match_context ~line ~anchor_pos:stack_top.stack_end_anchor_pos
        ~default_re:stack_top.stack_end_re
        ~parent_anchor_re:stack_top.stack_end_re_parent_anchor
    in
    let end_match =
      match match_end_at ~re ~offset ~line_for_match ~pos with
      | None -> None
      | Some (region, start, end_) ->
        assert (start = pos);
        let toks =
          emit_end_captures ~line ~re ~offset
            ~tokenize_capture_patterns:run_capture_patterns
            ~capture_scopes:stack_top.stack_prev_scopes
            ~prefix_scopes:
              (add_scopes stack_top.stack_prev_scopes
                 [ delim.delim_name; delim.delim_content_name ])
            ~delim_name:delim.delim_name
            ~delim_end_captures:delim.delim_end_captures ~pos ~end_ ~region
            toks
        in
        Some (end_, toks)
    in
    match (delim.delim_kind, end_match) with
    | End, None -> k ()
    | End, Some (end_, toks') ->
      if not (has_progress pos end_) then
        if stack_top.stack_enter_pos >= pos then
          (* Zero-width end at or before line start for this stack frame:
             pop and force progress. *)
          match_line ~t ~grammar ~stack:stack' ~pos:(pos + 1) ~toks ~line
            (next_pats grammar stack')
        else
          (* Zero-width end later on the same line is deferred so the current
             scope remains active through end-of-line. *)
          k ()
      else
        let toks =
          emit_delim_close_token ~base_scopes:stack_top.stack_prev_scopes
            ~delim_name:delim.delim_name ~ending:end_ toks'
        in
        (* Pop the delimiter off the stack and continue *)
        match_line ~t ~grammar ~stack:stack' ~pos:end_ ~toks ~line
          (next_pats grammar stack')
    | While, _ -> error "Unreachable"
  in
  if pos > len then
    (* End of string reached *)
    (remove_empties ({ scopes; ending = len } :: toks), stack)
  else
    (* No patterns have matched, so increment the position and try again *)
    let k () =
      match_line ~t ~grammar:cur_grammar ~stack ~pos:(pos + 1) ~toks ~line
        stk_pats
    in
    match stack with
    | [] -> try_pats repos grammar rem_pats ~k
    | se :: stack' -> (
      match se.stack_delim.delim_kind with
      | While -> try_pats repos se.stack_grammar rem_pats ~k
      | End ->
        if se.stack_delim.delim_apply_end_pattern_last then
          try_pats repos se.stack_grammar rem_pats ~k:(fun () ->
              try_delim se stack' ~k)
        else
          try_delim se stack' ~k:(fun () ->
              try_pats repos se.stack_grammar rem_pats ~k))

and tokenize_exn t grammar stack line =
  (* See https://github.com/Microsoft/vscode-textmate/issues/25 for how to
     handle while rules. This is important for the Markdown grammar. *)
  let rec try_while_rules pos toks rem_stack = function
    | [] -> (toks, pos, rem_stack)
    | se :: stack -> (
      match se.stack_delim.delim_kind with
      | End -> try_while_rules pos toks (se :: rem_stack) stack
      | While ->
        let run_capture_patterns base_scopes patterns start end_ =
          tokenize_capture_patterns ~t ~repos:se.stack_repos ~line ~base_scopes
            ~start ~end_ patterns
        in
        let re, offset, line_for_match =
          select_end_match_context ~line ~anchor_pos:pos
            ~default_re:se.stack_end_re
            ~parent_anchor_re:se.stack_end_re_parent_anchor
        in
        let rec loop pos' =
          if pos' = String.length line then (toks, pos, rem_stack)
          else
            match match_end_at ~re ~offset ~line_for_match ~pos:pos' with
            | None -> loop (pos' + 1)
            | Some (region, start, end_) ->
              assert (start = pos');
              if not (has_progress pos' end_) then loop (pos' + 1)
              else
                let toks =
                  emit_end_captures ~line ~re ~offset
                    ~tokenize_capture_patterns:run_capture_patterns
                    ~capture_scopes:se.stack_scopes
                    ~prefix_scopes:se.stack_scopes ~delim_name:None
                    ~delim_end_captures:se.stack_delim.delim_end_captures
                    ~pos:pos' ~end_ ~region toks
                in
                let toks =
                  emit_delim_close_token ~base_scopes:se.stack_prev_scopes
                    ~delim_name:se.stack_delim.delim_name ~ending:end_ toks
                in
                try_while_rules end_ toks (se :: rem_stack) stack
        in
        loop pos)
  in
  let toks, pos, stack = try_while_rules 0 [] [] (List.rev stack) in
  match_line ~t ~grammar ~stack ~pos ~toks ~line (next_pats grammar stack)

and tokenize_capture_patterns ~t ~repos ~line ~base_scopes ~start ~end_
    patterns =
  if start >= end_ then []
  else
    let capture_scope_name = "__capture__" in
    let capture_grammar =
      {
        name = None;
        scope_name = capture_scope_name;
        filetypes = [];
        patterns;
        repository = merge_repos repos;
      }
    in
    let segment = String.sub line start (end_ - start) in
    let nested, _ = tokenize_exn t capture_grammar empty segment in
    let nested = List.rev nested in
    let normalize_scopes scopes =
      match List.rev scopes with
      | root :: rest when root = capture_scope_name -> List.rev rest
      | _ -> scopes
    in
    List.map
      (fun tok ->
        let scopes = normalize_scopes tok.scopes @ base_scopes in
        let ending = tok.ending + start in
        { ending; scopes })
      nested
