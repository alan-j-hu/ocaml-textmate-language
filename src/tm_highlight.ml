type capture = {
    capture_name : string;
  }

module IntMap = Map.Make(Int)

type match_ = {
    name : string option;
    pattern : Pcre.regexp;
    captures : capture IntMap.t;
  }

type delim = {
    delim_begin : Pcre.regexp;
    delim_end : Pcre.regexp;
    delim_patterns : pattern list;
    delim_name : string option;
    delim_content_name : string option;
    delim_begin_captures : capture IntMap.t;
    delim_end_captures : capture IntMap.t;
  }

and pattern_kind =
  | Match of match_
  | Delim of delim
  | Include of string

and pattern = {
    pattern_kind : pattern_kind;
  }

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

let of_plist_exn plist =
  let iflags = Pcre.cflags [`ANCHORED; `DOLLAR_ENDONLY] in
  let rec get_captures acc = function
    | [] -> acc
    | (k, v) :: kvs ->
       let idx = int_of_string k in
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
    let kind =
      match find "include" obj with
      | Some s -> Include (get_string s)
      | None ->
         match find "match" obj, find "begin" obj, find "end" obj with
         | Some s, None, None ->
            Match {
                pattern = Pcre.regexp ~iflags (get_string s);
                name = Option.map get_string (find "name" obj);
                captures =
                  match find "captures" obj with
                  | None -> IntMap.empty
                  | Some value ->
                     get_captures IntMap.empty (get_dict value)
              }
         | None, Some b, Some e ->
            let delim_begin_captures, delim_end_captures =
              match find "captures" obj with
              | Some value ->
                 let captures = get_captures IntMap.empty (get_dict value) in
                 captures, captures
              | None ->
                 ( (match find "beginCaptures" obj with
                    | Some value ->
                       get_captures IntMap.empty (get_dict value)
                    | None -> IntMap.empty)
                 , (match find "endCaptures" obj with
                    | Some value ->
                       get_captures IntMap.empty (get_dict value)
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
              }
         | _, _, _ -> error "Pattern must be either match or begin/end"
    in { pattern_kind = kind; }
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
         hashtbl
  }

type token =
  | Span of string option * int
  | Delim_open of delim * int
  | Delim_close of delim * int

(** If the stack is empty, returns the main patterns associated with the
    grammar. Otherwise, returns the patterns associated with the delimiter at
    the top of the stack. *)
let next_pats grammar = function
  | [] -> grammar.patterns
  | delim :: _ -> delim.delim_patterns

let handle_captures default substring =
  IntMap.fold (fun idx capture acc ->
      let start, end_ = Pcre.get_substring_ofs substring idx in
      (Span(Some capture.capture_name, end_)) :: (Span(default, start)) :: acc)

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
  let default = match stack with
    | [] -> None
    | x :: _ -> x.delim_name
  in
  (* Try each pattern in the list until one matches. If none match, increment
     [pos] and try all the patterns again. *)
  let rec try_pats ~k = function
    | [] -> k () (* No patterns have matched, so call the continuation *)
    | { pattern_kind = Match m } :: pats ->
       (try
          let subs = Pcre.exec ~pos ~rex:m.pattern line in
          let (start, end_) = Pcre.get_substring_ofs subs 0 in
          assert (start = pos);
          let acc = (Span(default, pos)) :: acc in
          let acc = handle_captures default subs m.captures acc in
          let acc = (Span(m.name, end_)) :: acc in
          match_line ~grammar ~stack ~len ~pos:end_ ~acc ~line
            (next_pats grammar stack)
        with Not_found -> try_pats ~k pats)
    | { pattern_kind = Delim d } :: pats ->
       (try
          (* Try to match the delimiter's begin pattern *)
          let subs = Pcre.exec ~pos ~rex:d.delim_begin line in
          let (start, end_) = Pcre.get_substring_ofs subs 0 in
          assert (start = pos);
          let acc = (Span(default, pos)) :: acc in
          let acc = handle_captures default subs d.delim_begin_captures acc in
          let acc = (Delim_open(d, end_)) :: acc in
          (* Push the delimiter on the stack and continue *)
          match_line ~grammar ~stack:(d :: stack) ~len ~pos:end_ ~acc ~line
            d.delim_patterns
        with Not_found -> try_pats ~k pats)
    | { pattern_kind = Include name } :: pats ->
       let k () = try_pats ~k pats in
       let len = String.length name in
       if name = "$self" then
         try_pats grammar.patterns ~k
       else if len > 0 && name.[0] = '#' then
         let key = String.sub name 1 (len - 1) in
         match Hashtbl.find_opt grammar.repository key with
         | None -> error ("Unknown repository key " ^ key)
         | Some pats -> try_pats pats ~k
       else
         error "Unimplemented"
  in
  if pos > len then
    (List.rev acc, stack) (* End of string reached *)
  else
    (* No patterns have matched, so increment the position and try again *)
    let k () =
      match_line ~grammar ~stack ~len ~pos:(pos + 1) ~acc ~line grammar.patterns
    in
    match stack with
    | [] -> try_pats rem_pats ~k
    | delim :: stack' ->
       (* Try to match the delimiter's end pattern *)
       let subs =
         try Some (Pcre.exec ~pos ~rex:delim.delim_end line) with
         | Not_found -> None
       in
       match subs with
       | None -> try_pats rem_pats ~k
       | Some subs ->
          let (start, end_) = Pcre.get_substring_ofs subs 0 in
          assert (start = pos);
          let acc = (Span(default, pos)) :: acc in
          let acc = handle_captures default subs delim.delim_end_captures acc in
          let acc = (Delim_close(delim, end_)) :: acc in
          (* Pop the delimiter off the stack and continue *)
          match_line ~grammar ~stack:stack' ~len ~pos:end_  ~acc ~line
            (next_pats grammar stack)

let tokenize_line grammar stack line =
  match_line ~grammar ~stack ~len:(String.length line) ~pos:0 ~acc:[] ~line
    (next_pats grammar stack)

module type Renderer = sig
  type span
  type line
  type block

  val create_span : string option -> string -> span
  val create_line : span list -> line
  val create_block : line list -> block
end

module type S = sig
  type line
  type block

  val highlight_line : grammar -> t -> string -> line * t
  val highlight_block : grammar -> string -> block
end

module Make (R : Renderer) = struct
  type line = R.line
  type block = R.block

  let create_span name i j line =
    assert (j >= i);
    let inner_text = String.sub line i (j - i) in
    R.create_span name inner_text

  let rec highlight_tokens stack i acc line = function
    | [] ->
       let name = match stack with
         | [] -> None
         | x :: _ -> x.delim_name
       in
       let span = create_span name i (String.length line) line in
       List.rev (span :: acc)
    | Span(name, j) :: toks ->
       let span = create_span name i j line in
       highlight_tokens stack j (span :: acc) line toks
    | Delim_open(d, j) :: toks ->
       let span = create_span d.delim_name i j line in
       highlight_tokens stack j (span :: acc) line toks
    | Delim_close(d, j) :: toks ->
       let span = create_span d.delim_name i j line in
       highlight_tokens stack j (span :: acc) line toks

  (** Maps over the list while keeping track of some state.

      Discards the state because I don't need it. *)
  let rec map_fold f acc = function
    | [] -> []
    | x :: xs ->
       let y, acc = f acc x in
       y :: map_fold f acc xs

  let highlight_line grammar stack line =
    (* Some patterns don't work if there isn't a newline *)
    let line = line ^ "\n" in
    let tokens, stack = tokenize_line grammar stack line in
    let spans = highlight_tokens stack 0 [] line tokens in
    R.create_line spans, stack

  let highlight_block grammar code =
    let lines = String.split_on_char '\n' code in
    let a's = map_fold (highlight_line grammar) [] lines in
    R.create_block a's
end
