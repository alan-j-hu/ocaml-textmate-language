(lang dune 2.7)
(generate_opam_files true)
(version 0.1.0)
(license MIT)
(authors "Alan Hu <hu.ala@northeastern.edu>")
(maintainers "Alan Hu <hu.ala@northeastern.edu>")
(source (github dosaylazy/ocaml-textmate-highlight))
(name textmate-highlight)

(package
  (name textmate-highlight)
  (synopsis "Syntax highlighting with TextMate grammars")
  (description "This library generates highlighted HTML fragments from TextMate
grammars.")
  (depends
    (pcre (and (>= 7.4) (< 8.0)))
    (plist-xml (< 0.3))))

(package
  (name textmate-highlight-ansi)
  (synopsis "Syntax highlighting with TextMate grammars")
  (description "This library generates ANSI highlighting from TextMate
grammars.")
  (depends
    (ANSITerminal (and (>= 0.8.2) (< 0.9)))
    (textmate-highlight (< 0.3))))


(package
  (name textmate-highlight-html)
  (synopsis "Syntax highlighting with TextMate grammars")
  (description "This library generates highlighted HTML fragments from TextMate
grammars.")
  (depends
    (lambdasoup (and (>= 0.7) (< 0.8)))
    (textmate-highlight (< 0.2))))
