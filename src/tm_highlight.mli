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

module type Renderer = sig
  type span
  type line
  type block

  val create_span : string option -> string -> span
  (** [create_span desc token] creates a span of highlighted code containing
      [token] belonging to the token category [desc]. *)

  val create_line : span list -> line
  (** [create_line spans] creates a highlighted line of code from the given
      list of spans. *)

  val create_block : line list -> block
  (** [create_block lines] creates a highlighted block of code from the given
      list of lines. *)
end
(** User-supplied highlighting backend for the highlighter. *)
