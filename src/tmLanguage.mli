type t
(** The collection of TextMate grammars. *)

type grammar
(** A TextMate grammar. *)

type stack
(** The state of the code highlighter. *)

exception Error of string
[@ocaml.warn_on_literal_pattern]
(** The error message is purely informational and is not to be matched on. *)

val create : unit -> t
(** Create an empty collection of grammars. *)

val add_grammar : t -> grammar -> unit
(** Add a grammar to the collection. *)

val find_by_name : t -> string -> grammar option
(** Finds a grammar by its [name] attribute. Case-insensitive. *)

val find_by_scope_name : t -> string -> grammar option
(** Finds for a grammar by its [scopeName] attribute. Case-sensitive. *)

val of_plist_exn : Plist_xml.t -> grammar
(** Reads a TextMate grammar from a plist file. Raises {!exception:Error} if
    the plist does not represent a valid TextMate grammar. *)

val empty : stack
(** The initial state of the code highlighter. *)

type token = {
    ending : int;
    (** The index of the character right after the last character of the
        token. *)
    scopes : string list; (** The token's stack of scopes. *)
  }
(** A token of code. If [token] is the first token of the line, it spans the
    substring from [0] to [token.ending]. If [token] succeeds a previous token
    [prev], [token] spans the substring from [prev.ending] to [token.ending]. *)

val tokenize_exn : t -> grammar -> stack -> string -> token list * stack
(** Usage: [let tokens, stack = tokenize_exn t grammar stack line]

    Tokenizes a line of code. Returns the list of tokens [tokens] and the
    updated tokenization state [stack].

    Precondition:

    - [line] must be a single line, including the newline character at the end.

    Postconditions:

    - [tokens] is always nonempty.
    - The [ending] field of the last token in [tokens] is always
      [String.length line].

    Throws {!exception:Error} if it tries to access a local grammar repository
    that doesn't exist. Currently, it silently ignores inclusions of other
    grammars that don't exist in [t]. *)
