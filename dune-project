(lang dune 2.6)
(generate_opam_files true)
(version 0.1.0)

(package
  (name textmate-highlight)
  (synopsis "Syntax highlighting with TextMate grammars")
  (description "This library generates highlighted HTML fragments from TextMate
grammars.")
  (depends
    (lambdasoup (and (>= 0.7) (< 0.8)))
    (pcre (and (>= 7.4) (< 8.0)))
    (plist-xml (< 0.2))))
