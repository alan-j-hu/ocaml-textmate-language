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
    (pcre (and (>= 7.3) (< 8.0))))
  (synopsis "Tokenizing source code with TextMate grammars")
  (description "
Tokenizing source code with TextMate grammars.

Note that this library uses PCRE regex, while TextMate uses the Oniguruma
dialect."))
