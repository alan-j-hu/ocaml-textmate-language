type grammar
(** The type of a TextMate grammar. *)

type stack
(** The type that keeps track of the stack of delimiters across lines. *)

exception Highlight_error of string
[@ocaml.warn_on_literal_pattern]
(** The error message is purely informational and is not to be matched on. *)

val of_plist_exn : Plist_xml.t -> grammar
(** May raise [Highlight_error]. *)

val empty : stack

val highlight_line :
  grammar -> stack -> string -> Soup.element Soup.node * stack

val highlight_block : grammar -> string -> Soup.element Soup.node
