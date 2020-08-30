type t
(** The collection of TextMate grammars. *)

type grammar
(** A TextMate grammar. *)

type stack
(** The state of the code highlighter. *)

exception Highlight_error of string
[@ocaml.warn_on_literal_pattern]
(** The error message is purely informational and is not to be matched on. *)

val create : unit -> t

val add_grammar : t -> grammar -> unit

val find_by_name : t -> string -> grammar option
(** Searches for a grammar by its name field. Case-insensitive. *)

val find_by_scope_name : t -> string -> grammar option
(** Searches for a grammar by its scopeName field. Case-sensitive. *)

val of_plist_exn : Plist_xml.t -> grammar
(** Reads a TextMate grammar from a plist file. May raise [Highlight_error]. *)

val empty : stack
(** The initial state of the code highlighter. *)

type token = {
    scopes : string list;
    ending : int;
  }

val tokenize_line : t -> grammar -> stack -> string -> token list * stack
