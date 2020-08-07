type span
type line
type block

module Renderer : sig
  include Tm_highlight.RENDERER
    with type span = span
     and type line = line
     and type block = block
end

type printer = ANSITerminal.style list -> string -> unit

val print_block : printer -> block -> unit
