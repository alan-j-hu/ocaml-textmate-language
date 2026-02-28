(lang dune 2.7)
(generate_opam_files true)
(authors "Alan Hu <alanh@ccs.neu.edu>")
(maintainers "Alan Hu <alanh@ccs.neu.edu>")
(license MIT)
(source (github alan-j-hu/ocaml-textmate-language))
(name textmate-language)

(package
 (name textmate-language)
 (depends
  (oniguruma (>= 0.1.2))
  (ocaml (>= 4.08))
  (plist-xml (and (>= 0.5) :with-test))
  (alcotest (and (>= 1.4) :with-test))
  (ezjsonm (and (>= 1.2) :with-test))
  (yojson (and (>= 1.7) :with-test)))
 (synopsis "Tokenizing code with TextMate grammars for syntax highlighting")
 (tags (highlighting))
 (description "
This package provides functions for reading TextMate grammars and tokenizing
code on a line-by-line basis. `textmate-language` can read grammars from the
document types of the `plist-xml`, `ezjsonm`, and `yojson` libraries."))
