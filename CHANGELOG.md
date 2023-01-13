# Changelog

## 0.2.0.0

* BREAKING: field interpolation now uses `{}`, *without* a dollar sign (before: `${}`)
* Add support for row interpolation (uses `@{}` syntax, see the updated readme for details)
* Refactor the parser into a [custom-interpolation](https://hackage.haskell.org/package/custom-interpolation) package which does most of the work

## 0.1.1

* Add support for textual SQL injection (PR #1)
* Support GHC 8.10.7

## 0.1.0

* Initial version.
