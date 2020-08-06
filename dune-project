(lang dune 2.6)
(generate_opam_files true)
(version 0.1.0)
(license MIT)
(authors "Alan Hu <hu.ala@northeastern.edu>")
(maintainers "Alan Hu <hu.ala@northeastern.edu>")
(source (github dosaylazy/ocaml-textmate-highlight))

(package
  (name textmate-highlight)
  (synopsis "Syntax highlighting with TextMate grammars")
  (description "This library generates highlighted HTML fragments from TextMate
grammars.")
  (depends
    (lambdasoup (and (>= 0.7) (< 0.8)))
    (odoc :with-doc)
    (pcre (and (>= 7.4) (< 8.0)))
    (plist-xml (< 0.2))))
