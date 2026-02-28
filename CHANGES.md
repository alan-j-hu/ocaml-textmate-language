## 0.5.0 (February 28, 2026)

- Merge PR "Fix OCaml grammar compatibility" (#1) by @davesnx
- Handle captures specified as lists instead of dictionaries

## 0.4.0 (January 3, 2024)

- Rewrite handling of captures, previous version returned indices out of order

## 0.3.4 (March 14, 2023)

- Update the test dependency `plist-xml` to version 0.5.

## 0.3.3 (October 27, 2022)

- Support named capture groups.
- Fix mapping scopes to captures.

## 0.3.2 (September 28, 2022)

- Fix bug of incorrect, non-increasing string indices being returned.

## 0.3.1 (July 15, 2021)

- Add `find_by_filetype` function.

## 0.3.0 (July 4, 2021)

- Fix handling of while
- Fix handling of end-delimiter scopes
- Add type definitions for [Ezjsonm](https://opam.ocaml.org/packages/ezjsonm/)
  and [Yojson](https://opam.ocaml.org/packages/yojson/) JSON values
- Add functions for reading JSON-format grammars into Ezjsonm or Yojson
  values
- Add tests

## 0.2.1 (April 9, 2021)

- Check capture index bounds and ignore out-of-bounds captures indices in
  rules. Previously the underlying Oniguruma bindings would throw an
  `Invalid_argument` exception.

## 0.2.0 (November 26, 2020)

- Switch from PCRE to Oniguruma.
- Substitute captures from `begin` pattern for backreferences in `end` and
  `while` patterns.

## 0.1.0 (August 30, 2020)

Initial release.
