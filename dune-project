(lang dune 2.7)
(generate_opam_files true)
(version 0.1.0)
(authors "Alan Hu <hu.ala@northeastern.edu>")
(maintainers "Alan Hu <hu.ala@northeastern.edu>")
(license MIT)
(documentation https://alan-j-hu.github.io/ocaml-textmate-language/)
(source (github alan-j-hu/ocaml-textmate-language))
(name textmate-language)

(package
 (name textmate-language)
 (depends
  (oniguruma (< 0.2.0)))
 (synopsis "Tokenizing source code with TextMate grammars")
 (description "
Tokenizing source code with TextMate grammars."))