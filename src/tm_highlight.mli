type grammar
(** The type of a TextMate grammar. *)

type t
(** The state of the code highlighter. *)

exception Highlight_error of string
[@ocaml.warn_on_literal_pattern]
(** The error message is purely informational and is not to be matched on. *)

val of_plist_exn : Plist_xml.t -> grammar
(** Reads a TextMate grammar from a plist file. May raise [Highlight_error]. *)

val empty : t
(** The initial state of the code highlighter. *)

type token = {
    scope : string option;
    ending : int;
  }

val tokenize_line : grammar -> t -> string -> token list * t
