(lang dune 2.7)
(generate_opam_files true)
(version 0.1.0)
(license MIT)
(authors "Alan Hu <hu.ala@northeastern.edu>")
(maintainers "Alan Hu <hu.ala@northeastern.edu>")
(source (github dosaylazy/ocaml-textmate-language))

(package
  (name textmate-language)
  (synopsis "Parsing source code with TextMate grammars")
  (description "Parsing source code with TextMate grammars.")
  (depends
    (pcre (and (>= 7.4) (< 8.0)))
    (plist-xml (< 0.3))))
