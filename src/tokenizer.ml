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
let handle_captures ?(region_offset = 0) re scopes default mat_start mat_end
    region captures tokens =
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
  let _, _, stack, tokens =
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
  (* Try each pattern in the list until one matches. If none match, increment
     [pos] and try all the patterns again. *)
  let rec try_pats repos cur_grammar ~k = function
    | [] -> k () (* No patterns have matched, so call the continuation *)
    | Match m :: pats -> (
      match match_regex m.pattern line pos Oniguruma.Options.none with
      | No_match | Empty_match _ -> try_pats repos cur_grammar ~k pats
      | Nonempty_match { region; end_ } ->
        let toks = { scopes; ending = pos } :: toks in
        let toks =
          handle_captures m.pattern scopes m.name pos end_ region m.captures
            toks
        in
        let toks =
          { scopes = add_scopes scopes [ m.name ]; ending = end_ } :: toks
        in
        match_line ~t ~grammar ~stack ~pos:end_ ~toks ~line
          (next_pats grammar stack))
    | Delim d :: pats -> (
      (* Try to match the delimiter's begin pattern *)
      match match_regex d.delim_begin line pos Oniguruma.Options.none with
      | No_match -> try_pats repos cur_grammar ~k pats
      | Empty_match _ when has_same_delim_at_pos stack d pos ->
        match_line ~t ~grammar ~stack ~pos:(pos + 1) ~toks ~line
          (next_pats grammar stack)
      | Empty_match { region; end_ } | Nonempty_match { region; end_ } -> (
        let toks = { scopes; ending = pos } :: toks in
        let toks =
          handle_captures d.delim_begin scopes d.delim_name pos end_ region
            d.delim_begin_captures toks
        in
        let toks =
          { scopes = add_scopes scopes [ d.delim_name ]; ending = end_ }
          :: toks
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
          (* Push the delimiter on the stack and continue *)
          match_line ~t ~grammar ~stack:(se :: stack) ~pos:end_ ~toks ~line
            d.delim_patterns
        | While ->
          (* Subsume the remainder of the line into a span *)
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
      let k () = try_pats repos cur_grammar ~k pats in
      try_pats repos cur_grammar child_patterns ~k
    | Include_scope name :: pats -> (
      match find_by_scope_name t name with
      | None ->
        (* Grammar not found; try the next pattern. *)
        try_pats repos cur_grammar ~k pats
      | Some nested_grammar ->
        let k () = try_pats repos cur_grammar ~k pats in
        try_pats
          [ nested_grammar.repository ]
          nested_grammar nested_grammar.patterns ~k)
    | Include_base :: pats ->
      let k () = try_pats repos cur_grammar ~k pats in
      try_pats [ grammar.repository ] grammar grammar.patterns ~k
    | Include_self :: pats ->
      let k () = try_pats repos cur_grammar ~k pats in
      try_pats [ cur_grammar.repository ] cur_grammar cur_grammar.patterns ~k
    | Include_local key :: pats -> (
      match find_nested key repos with
      | None -> try_pats repos cur_grammar ~k pats
      | Some item -> (
        match item.repo_item_kind with
        | Repo_rule rule ->
          try_pats (item.repo_inner :: repos) cur_grammar (rule :: pats) ~k
        | Repo_patterns pats' ->
          let k () = try_pats repos cur_grammar ~k pats in
          try_pats (item.repo_inner :: repos) cur_grammar pats' ~k))
  in
  let try_delim stack_top stack' ~k =
    (* Try to match the delimiter's end pattern *)
    let delim = stack_top.stack_delim in
    let match_end () =
      match stack_top.stack_end_re_parent_anchor with
      | None ->
        let re = stack_top.stack_end_re in
        let offset = 0 in
        (re, offset, line)
      | Some re ->
        let offset = stack_top.stack_end_anchor_pos in
        if offset >= String.length line then (stack_top.stack_end_re, 0, line)
        else
          let line_for_match =
            String.sub line offset (String.length line - offset)
          in
          (re, offset, line_for_match)
    in
    let emit_close_tokens re offset region end_ =
      let toks =
        {
          scopes =
            add_scopes stack_top.stack_prev_scopes
              [ delim.delim_name; delim.delim_content_name ];
          ending = pos;
        }
        :: toks
      in
      handle_captures ~region_offset:offset re stack_top.stack_prev_scopes
        delim.delim_name pos end_ region delim.delim_end_captures toks
    in
    let end_match =
      let re, offset, line_for_match = match_end () in
      let pos_for_match = pos - offset in
      if pos_for_match < 0 then None
      else
        match
          match_regex re line_for_match pos_for_match Oniguruma.Options.none
        with
        | No_match -> None
        | Empty_match { region; end_ } ->
          let end_ = end_ + offset in
          let toks = emit_close_tokens re offset region end_ in
          Some (`Empty, end_, toks)
        | Nonempty_match { region; end_ } ->
          let end_ = end_ + offset in
          let toks = emit_close_tokens re offset region end_ in
          Some (`Nonempty, end_, toks)
    in
    match (delim.delim_kind, end_match) with
    | End, None -> k ()
    | End, Some (`Empty, end_, toks) ->
      if stack_top.stack_enter_pos = pos then
        (* Zero-width end at its enter point: pop and force progress. *)
        match_line ~t ~grammar ~stack:stack' ~pos:(pos + 1) ~toks ~line
          (next_pats grammar stack')
      else
        let toks =
          { scopes = add_scopes scopes [ delim.delim_name ]; ending = end_ }
          :: toks
        in
        (* Pop the delimiter off the stack and continue *)
        match_line ~t ~grammar ~stack:stack' ~pos:end_ ~toks ~line
          (next_pats grammar stack')
    | End, Some (`Nonempty, end_, toks) ->
      let toks =
        { scopes = add_scopes scopes [ delim.delim_name ]; ending = end_ }
        :: toks
      in
      (* Pop the delimiter off the stack and continue *)
      match_line ~t ~grammar ~stack:stack' ~pos:end_ ~toks ~line
        (next_pats grammar stack')
    | While, _ -> error "Unreachable"
  in
  if pos > len then
    (* End of string reached *)
    match stack with
    | [] -> (remove_empties ({ scopes; ending = len } :: toks), stack)
    | se :: _stack' ->
      let d = se.stack_delim in
      ( remove_empties
          ({ scopes = add_scopes scopes [ d.delim_name ]; ending = len }
          :: toks),
        stack )
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

let tokenize_exn t grammar stack line =
  (* See https://github.com/Microsoft/vscode-textmate/issues/25 for how to
     handle while rules. This is important for the Markdown grammar. *)
  let rec try_while_rules pos toks rem_stack = function
    | [] -> (toks, pos, rem_stack)
    | se :: stack -> (
      match se.stack_delim.delim_kind with
      | End -> try_while_rules pos toks (se :: rem_stack) stack
      | While ->
        let re = se.stack_end_re in
        let rec loop pos' =
          if pos' = String.length line then (toks, pos, rem_stack)
          else
            match match_regex re line pos' Oniguruma.Options.none with
            | No_match | Empty_match _ -> loop (pos' + 1)
            | Nonempty_match { region; end_ } ->
              let toks =
                { scopes = se.stack_prev_scopes; ending = pos' } :: toks
              in
              let toks =
                handle_captures re se.stack_prev_scopes
                  se.stack_delim.delim_name pos' end_ region
                  se.stack_delim.delim_end_captures toks
              in
              let toks =
                {
                  scopes =
                    add_scopes se.stack_prev_scopes
                      [ se.stack_delim.delim_name ];
                  ending = end_;
                }
                :: toks
              in
              try_while_rules end_ toks (se :: rem_stack) stack
        in
        loop pos)
  in
  let toks, pos, stack = try_while_rules 0 [] [] (List.rev stack) in
  match_line ~t ~grammar ~stack ~pos ~toks ~line (next_pats grammar stack)
