## 0.3.0 (July 3, 2021)

- Fix handling of while
- Fix handling of end-delimiter scopes
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
