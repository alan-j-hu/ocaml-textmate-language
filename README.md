The OCaml grammar located in `bin` was taken from
https://github.com/textmate/ocaml.tmbundle.

To feed the highlighter its own dogfood:

    cat bin/main.ml | dune exec bin/main.exe ocaml bin/OCaml.plist
