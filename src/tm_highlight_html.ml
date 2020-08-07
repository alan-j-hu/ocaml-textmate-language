let get_classes spec =
  let toks = String.split_on_char '.' spec in
  let _, classes =
    List.fold_left (fun (toks, classes) tok ->
        let toks = tok :: toks in
        (toks, (String.concat "." (List.rev toks)) :: classes)
      ) ([], []) toks
  in classes

module Renderer = struct
  type span = Node : 'a Soup.node -> span
  type line = Soup.element Soup.node
  type block = Soup.element Soup.node

  let create_span name inner_text =
    let classes = Option.map get_classes name in
    match classes with
    | None -> Node (Soup.create_text inner_text)
    | Some classes -> Node (Soup.create_element ~classes "span" ~inner_text)

  let create_line nodes =
    let a = Soup.create_element "a" ~class_:"sourceLine" in
    List.iter (fun (Node node) -> Soup.append_child a node) nodes;
    a

  let create_block a's =
    let code = Soup.create_element "code" in
    List.iter (Soup.append_child code) a's;
    let pre = Soup.create_element "pre" in
    Soup.append_child pre code;
    pre
end
