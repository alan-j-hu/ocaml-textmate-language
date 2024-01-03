open Common

let find_exn key obj =
  match List.assoc_opt key obj with
  | Some v -> v
  | None -> error (key ^ " not found.")

let get_dict = function
  | `Assoc d | `Dict d | `O d -> d
  | _ -> error "Type error: Expected dict."

let get_string = function
  | `String s -> s
  | _ -> error "Type error: Expected string."

let get_list f = function
  | `A l | `Array l | `List l -> List.map f l
  | _ -> error "Type error: Expected list."

let compile_regex re =
  match
    Oniguruma.create re Oniguruma.Options.none Oniguruma.Encoding.utf8
      Oniguruma.Syntax.default
  with
  | Error msg -> error (re ^ ": " ^ msg)
  | Ok re -> re

let rec get_captures kvs =
  let tbl = Hashtbl.create 21 in
  let rec loop = function
    | [] -> ()
    | (k, v) :: kvs ->
      let cap =
        match int_of_string_opt k with
        | Some int -> Capture_idx int
        | None -> Capture_name k
      in
      let v = get_dict v in
      let capture_name =
        match List.assoc_opt "name" v with
        | None -> None
        | Some name -> Some (get_string name)
      in
      let capture_patterns =
        match List.assoc_opt "patterns" v with
        | None -> []
        | Some v -> get_pattern_list v
      in
      Hashtbl.replace tbl cap { capture_name; capture_patterns };
      loop kvs
  in
  loop kvs;
  tbl

and get_pattern_list l = get_list (fun x -> patterns_of_plist (get_dict x)) l
and get_patterns obj = find_exn "patterns" obj |> get_pattern_list

and patterns_of_plist obj =
  match List.assoc_opt "include" obj with
  | Some s -> (
    match get_string s with
    | "$base" -> Include_base
    | "$self" -> Include_self
    | s ->
      let len = String.length s in
      if len > 0 && s.[0] = '#' then Include_local (String.sub s 1 (len - 1))
      else Include_scope s)
  | None -> (
    match (List.assoc_opt "match" obj, List.assoc_opt "begin" obj) with
    | Some s, None ->
      Match
        {
          pattern = compile_regex (get_string s);
          name = Option.map get_string (List.assoc_opt "name" obj);
          captures =
            (match List.assoc_opt "captures" obj with
            | None -> Hashtbl.create 0
            | Some value -> get_captures (get_dict value));
        }
    | None, Some b ->
      let e, key, delim_kind =
        match (List.assoc_opt "end" obj, List.assoc_opt "while" obj) with
        | Some e, None -> (e, "endCaptures", End)
        | None, Some e -> (e, "whileCaptures", While)
        | _, _ -> error "Begin patterns must either have an end or while."
      in
      let delim_begin_captures, delim_end_captures =
        match List.assoc_opt "captures" obj with
        | Some value ->
          let captures = get_captures (get_dict value) in
          (captures, captures)
        | None ->
          ( (match List.assoc_opt "beginCaptures" obj with
            | Some value -> get_captures (get_dict value)
            | None -> Hashtbl.create 0),
            match List.assoc_opt key obj with
            | Some value -> get_captures (get_dict value)
            | None -> Hashtbl.create 0 )
      in
      Delim
        {
          delim_begin = compile_regex (get_string b);
          delim_end = get_string e;
          delim_patterns =
            (match List.assoc_opt "patterns" obj with
            | None -> []
            | Some v -> get_pattern_list v);
          delim_name = Option.map get_string (List.assoc_opt "name" obj);
          delim_content_name =
            Option.map get_string (List.assoc_opt "contentName" obj);
          delim_begin_captures;
          delim_end_captures;
          delim_apply_end_pattern_last =
            (match List.assoc_opt "applyEndPatternLast" obj with
            | Some (`Int 1) -> true
            | _ -> false);
          delim_kind;
        }
    | _, _ -> error "Pattern must be match, begin/end, or begin/while.")

let of_doc_exn (plist : union) =
  let rec get_repo_item obj =
    {
      repo_item_kind =
        (match (List.assoc_opt "match" obj, List.assoc_opt "begin" obj) with
        | None, None -> Repo_patterns (get_patterns obj)
        | _, _ -> Repo_rule (patterns_of_plist obj));
      repo_inner =
        (match List.assoc_opt "repository" obj with
        | None -> Hashtbl.create 0
        | Some obj -> get_repo obj);
    }
  and get_repo obj =
    let hashtbl = Hashtbl.create 31 in
    List.iter
      (fun (k, v) ->
        let v = get_dict v in
        let item = get_repo_item v in
        Hashtbl.add hashtbl k item)
      (get_dict obj);
    hashtbl
  in
  let obj = get_dict plist in
  {
    name = get_string (find_exn "name" obj);
    scope_name = get_string (find_exn "scopeName" obj);
    filetypes =
      (match List.assoc_opt "fileTypes" obj with
      | None -> []
      | Some filetypes -> get_list get_string filetypes);
    patterns = get_patterns obj;
    repository =
      (match List.assoc_opt "repository" obj with
      | None -> Hashtbl.create 0
      | Some obj -> get_repo obj);
  }

let of_plist_exn = (of_doc_exn :> plist -> grammar)
let of_ezjsonm_exn = (of_doc_exn :> ezjsonm -> grammar)
let of_yojson_exn = (of_doc_exn :> yojson -> grammar)
