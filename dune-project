(lang dune 2.7)
(generate_opam_files true)
(authors "Alan Hu <alanh@ccs.neu.edu>")
(maintainers "Alan Hu <alanh@ccs.neu.edu>")
(license MIT)
(documentation https://alan-j-hu.github.io/ocaml-textmate-language/)
(source (github alan-j-hu/ocaml-textmate-language))
(name textmate-language)

(package
 (name textmate-language)
 (depends
  (ocaml (>= 4.08))
  (oniguruma (< 0.2.0))
  (plist-xml :with-test)
  (alcotest (and (>= 1.4) (< 2) :with-test))
  (ezjsonm (and (>= 1.2) (< 2) :with-test))
  (yojson (and (>= 1.7) (< 2) :with-test)))
 (synopsis "Tokenizing source code with TextMate grammars")
 (description "
Tokenizing source code with TextMate grammars."))
