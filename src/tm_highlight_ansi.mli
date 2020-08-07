type line
type block

module Renderer :
Tm_highlight.Renderer with type line = line and type block = block

type style = string -> ANSITerminal.style list option
type printer = ANSITerminal.style list -> string -> unit

val print_block : style -> printer -> block -> unit
