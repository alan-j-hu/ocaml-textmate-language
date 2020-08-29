type capture = {
    capture_name : string;
  }

module IntMap = Map.Make(Int)

type match_ = {
    name : string option;
    pattern : Pcre.regexp;
    captures : capture IntMap.t;
  }

type delim_kind = End | While

type delim = {
    delim_begin : Pcre.regexp;
    delim_end : Pcre.regexp;
    delim_patterns : pattern list;
    delim_name : string option;
    delim_content_name : string option;
    delim_begin_captures : capture IntMap.t;
    delim_end_captures : capture IntMap.t;
    delim_apply_end_pattern_last : bool;
    delim_kind : delim_kind;
  }

and pattern =
  | Match of match_
  | Delim of delim
  | Include_local of string
  | Include_scope of string
  | Include_self

type grammar = {
    name : string;
    patterns : pattern list;
    repository : (string, pattern list) Hashtbl.t;
  }

exception Highlight_error of string

let error msg = raise (Highlight_error msg)

let rec find key = function
  | [] -> None
  | (k, v) :: obj ->
     if k = key then
       Some v
     else
       find key obj

let find_exn key obj =
  match find key obj with
  | Some v -> v
  | None -> error (key ^ " not found")

let get_dict = function
  | `Dict d -> d
  | _ -> error "Type error: Expected dict"

let get_string = function
  | `String s -> s
  | _ -> error "Type error: Expected string"

let get_list f = function
  | `Array l -> List.map f l
  | _ -> error "Type error: Expected list"

let iflags = Pcre.cflags [`ANCHORED; `DOLLAR_ENDONLY]

let of_plist_exn plist =
  let rec get_captures acc = function
    | [] -> acc
    | (k, v) :: kvs ->
       let idx = match int_of_string_opt k with
         | Some int -> int
         | None -> error (k ^ " is not an integer")
       in
       let v = get_dict v in
       let name = match find "name" v with
         | None -> error "No name key in capture"
         | Some name -> get_string name
       in get_captures (IntMap.add idx { capture_name = name } acc) kvs
  in
  let rec get_patterns obj =
    get_list (fun x -> get_dict x |> patterns_of_plist)
      (find_exn "patterns" obj)
  and patterns_of_plist obj =
    match find "include" obj with
    | Some s ->
       let s = get_string s in
       let len = String.length s in
       if s = "$self" then
         Include_self
       else if len > 0 && s.[0] = '#' then
         Include_local (String.sub s 1 (len - 1))
       else
         Include_scope s
    | None ->
       match find "match" obj, find "begin" obj with
       | Some s, None ->
          Match {
              pattern = Pcre.regexp ~iflags (get_string s);
              name = Option.map get_string (find "name" obj);
              captures =
                match find "captures" obj with
                | None -> IntMap.empty
                | Some value -> get_captures IntMap.empty (get_dict value)
            }
       | None, Some b ->
          let e, key, delim_kind = match find "end" obj, find "while" obj with
            | Some e, None -> e, "endCaptures", End
            | None, Some e -> e, "whileCaptures", While
            | _, _ -> error "Begin pattern must either have end and while"
          in
          let delim_begin_captures, delim_end_captures =
            match find "captures" obj with
            | Some value ->
               let captures = get_captures IntMap.empty (get_dict value) in
               captures, captures
            | None ->
               ( (match find "beginCaptures" obj with
                  | Some value -> get_captures IntMap.empty (get_dict value)
                  | None -> IntMap.empty)
               , (match find key obj with
                  | Some value -> get_captures IntMap.empty (get_dict value)
                  | None -> IntMap.empty) )
          in
          Delim {
              delim_begin = Pcre.regexp ~iflags (get_string b);
              delim_end = Pcre.regexp ~iflags (get_string e);
              delim_patterns =
                begin match find "patterns" obj with
                | None -> []
                | Some v ->
                   get_list (fun x -> get_dict x |> patterns_of_plist) v
                end;
              delim_name = Option.map get_string (find "name" obj);
              delim_content_name =
                Option.map get_string (find "contentName" obj);
              delim_begin_captures;
              delim_end_captures;
              delim_apply_end_pattern_last =
                begin match find "applyEndPatternLast" obj with
                | Some (`Int 1) -> true
                | _ -> false
                end;
              delim_kind;
            }
       | _, _ -> error "Pattern must be match, begin/end, or begin/while"
  in
  let obj = get_dict plist in
  { name = get_string (find_exn "name" obj)
  ; patterns = get_patterns obj
  ; repository =
      match find "repository" obj with
      | None -> Hashtbl.create 0
      | Some kvs ->
         let hashtbl = Hashtbl.create 31 in
         List.iter (fun (k, v) ->
             let v = get_dict v in
             let pats = get_patterns v in
             Hashtbl.add hashtbl k pats
           ) (get_dict kvs);
         hashtbl }

type token = {
    scope : string option;
    ending : int;
  }

(** If the stack is empty, returns the main patterns associated with the
    grammar. Otherwise, returns the patterns associated with the delimiter at
    the top of the stack. *)
let next_pats grammar = function
  | [] -> grammar.patterns
  | delim :: _ -> delim.delim_patterns

let handle_captures default mat_end line captures tokens =
  let _, tokens =
    (* Regex captures are ordered by their left parentheses. Do a depth-first
       preorder traversal by keeping a stack of captures. *)
    IntMap.fold (fun idx capture (stack, tokens) ->
        (* If the capture mentions a lookahead, it can go past the bounds of its
           parent. The match is capped at the boundary for the parent. Is this
           the right decision to make? Clearly the writer of the grammar
           intended for the capture to exceed the parent in this case. *)
        match stack with
        | [] ->
           (try
              let cap_start, cap_end = Pcre.get_substring_ofs line idx in
              let cap_end =
                if cap_end > mat_end then
                  mat_end
                else
                  cap_end
              in
              ( [cap_end, capture.capture_name]
              , { scope = Some capture.capture_name; ending = cap_end }
                :: { scope = default; ending = cap_start } :: tokens )
            with Not_found | Invalid_argument _ -> ([], tokens))
        | (top_end, top_name) :: stack' ->
           try
             let cap_start, cap_end = Pcre.get_substring_ofs line idx in
             if cap_start >= top_end then
               let under = match stack' with
                 | [] -> mat_end
                 | (n, _) :: _ -> n
               in
               let cap_end =
                 if cap_end > under then
                   under
                 else
                   cap_end
               in
               ( (cap_end, capture.capture_name) :: stack'
               , { scope = Some capture.capture_name; ending = cap_end }
                 :: { scope = Some top_name; ending = cap_start } :: tokens )
             else
               let cap_end =
                 if cap_end > top_end then
                   top_end
                 else
                   cap_end
               in
               ( (cap_end, capture.capture_name) :: stack
               , { scope = Some capture.capture_name; ending = cap_end }
                 :: { scope = Some top_name; ending = cap_start } :: tokens )
           with Not_found | Invalid_argument _ -> (stack, tokens)
      ) captures ([], tokens)
  in tokens

type t = delim list

let empty = []

(** Tokenizes a line according to the grammar.

    [grammar]: The language grammar.
    [stack]: The stack that keeps track of nested delimiters
    [len]: The length of the string.
    [pos]: The current index into the string.
    [acc]: The list of tokens, with the rightmost ones at the front.
    [line]: The string that is being matched and tokenized.
    [rem_pats]: The remaining patterns yet to be tried *)
let rec match_line ~grammar ~stack ~len ~pos ~acc ~line rem_pats =
  let default, stk_pats = match stack with
    | [] -> None, grammar.patterns
    | d :: _ ->
       ( (match d.delim_content_name with
          | Some name -> Some name
          | None -> d.delim_name)
       , d.delim_patterns )
  in
  (* Try each pattern in the list until one matches. If none match, increment
     [pos] and try all the patterns again. *)
  let rec try_pats ~k = function
    | [] -> k () (* No patterns have matched, so call the continuation *)
    | Match m :: pats ->
       begin match Pcre.exec ~pos ~rex:m.pattern line with
       | exception Not_found -> try_pats ~k pats
       | subs ->
          let start, end_ = Pcre.get_substring_ofs subs 0 in
          assert (start = pos);
          let acc = { scope = default; ending = pos } :: acc in
          let acc = handle_captures default end_ subs m.captures acc in
          let acc = { scope = m.name; ending = end_ } :: acc in
          match_line ~grammar ~stack ~len ~pos:end_ ~acc ~line
            (next_pats grammar stack)
       end
    | Delim d :: pats ->
       (* Try to match the delimiter's begin pattern *)
       begin match Pcre.exec ~pos ~rex:d.delim_begin line with
       | exception Not_found -> try_pats ~k pats
       | subs ->
          let start, end_ = Pcre.get_substring_ofs subs 0 in
          assert (start = pos);
          let acc = { scope = default; ending = pos } :: acc in
          let acc =
            handle_captures d.delim_name end_ subs d.delim_begin_captures acc
          in
          let acc = { scope = d.delim_name; ending = end_ } :: acc in
          match d.delim_kind with
          | End ->
             (* Push the delimiter on the stack and continue *)
             match_line ~grammar ~stack:(d :: stack) ~len ~pos:end_ ~acc ~line
               d.delim_patterns
          | While ->
             (* Subsume the remainder of the line into a span *)
             ( List.rev
                 ({ scope = d.delim_name; ending = String.length line } :: acc)
             , d :: stack )
       end
    | Include_scope _ :: _ -> error "Unimplemented"
    | Include_self :: pats ->
       let k () = try_pats ~k pats in
       try_pats grammar.patterns ~k
    | Include_local key :: pats ->
       let k () = try_pats ~k pats in
       match Hashtbl.find_opt grammar.repository key with
       | None -> error ("Unknown repository key " ^ key)
       | Some pats -> try_pats pats ~k
  in
  let try_delim delim stack' ~k =
    (* Try to match the delimiter's end pattern *)
    let end_match =
      match Pcre.exec ~pos ~rex:delim.delim_end line with
      | exception Not_found -> None
      | subs ->
         let start, end_ = Pcre.get_substring_ofs subs 0 in
         assert (start = pos);
         let name = match delim.delim_content_name with
           | Some name -> Some name
           | None -> delim.delim_name
         in
         let acc = { scope = name; ending = pos } :: acc in
         let acc =
           handle_captures delim.delim_name end_ subs
             delim.delim_end_captures acc
         in Some (end_, acc)
    in
    match delim.delim_kind, end_match with
    | End, None -> k ()
    | End, Some (end_, acc) ->
       let acc = { scope = delim.delim_name; ending = end_ } :: acc in
       (* Pop the delimiter off the stack and continue *)
       match_line ~grammar ~stack:stack' ~len ~pos:end_  ~acc ~line
         (next_pats grammar stack)
    | While, Some (_, acc) ->
       (* Subsume the remainder of the line into a span *)
       ( List.rev
           ({ scope = delim.delim_name; ending = String.length line } :: acc)
       , stack )
    | While, None -> k ()
  in
  if pos > len then
    (* End of string reached *)
    match stack with
    | [] -> (List.rev ({ scope = None; ending = len } :: acc), stack)
    | { delim_kind = End; delim_name = scope; _ } :: _ ->
       (List.rev ({ scope; ending = len } :: acc), stack)
    (* If reached, this means that the while pattern wasn't matched. Retry the
       line. *)
    | { delim_kind = While; _ } :: stack ->
       match_line ~grammar ~stack ~len ~pos:0 ~acc:[] ~line
         (next_pats grammar stack)
  else
    (* No patterns have matched, so increment the position and try again *)
    let k () =
      match_line ~grammar ~stack ~len ~pos:(pos + 1) ~acc ~line stk_pats
    in
    match stack with
    | [] -> try_pats rem_pats ~k
    | delim :: stack' ->
       if delim.delim_apply_end_pattern_last then
         try_pats rem_pats ~k:(fun () -> try_delim delim stack' ~k)
       else
         try_delim delim stack' ~k:(fun () -> try_pats rem_pats ~k)

let tokenize_line grammar stack line =
  match_line ~grammar ~stack ~len:(String.length line) ~pos:0 ~acc:[] ~line
    (next_pats grammar stack)
