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

module type RENDERER = sig
  type span
  type line
  type block

  val create_span : string option -> int -> int -> string -> span
  (** [create_span desc start finish line] creates a span of highlighted
      code belonging to the token category [desc] for the substring of [line]
      in index range \[[start], [finish]) *)

  val create_line : span list -> line
  (** [create_line spans] creates a highlighted line of code from the given
      list of spans. *)

  val create_block : line list -> block
  (** [create_block lines] creates a highlighted block of code from the given
      list of lines. *)
end
(** User-supplied highlighting backend for the highlighter. *)

module type S = sig
  type line
  type block

  val highlight_line : grammar -> t -> string -> line * t

  val highlight_block : grammar -> string -> block
end

module Make (R : RENDERER) : S with type line = R.line and type block = R.block
