## 0.6.0.2

- Documentation fixes

## 0.6.0.1

- Haddock documentation fix

## 0.6.0.0

- Exceptions / exception handling.

## 0.5.3.0

- Marshalling and hoisting: it is now possible to fully marshal `GVal`s between
  arbitrary carrier monads, as long as suitable conversion functions are
  provided.

## 0.5.2.0

- Added map(), upper(), lower() functions

## 0.5.1.3

- Documentation fixes

## 0.5.1.2

- Release-related fixups

## 0.5.1.1

- Bugfixes wrt indentation mode

## 0.5.1.0

- Expose parser error pretty-printer from the library

## 0.5.0.0

- Indentation mode: `{% indent %}` introduces an indentation context

## 0.4.0.0

- Statements can now return values
- Added `do` expressions (lift statements into expressions)

## 0.3.11.1

- Fixed a parser bug related to whitespace in script mode

## 0.3.11.0

- Fixed the way local scopes work in script mode
- Documented script mode

## 0.3.10.0

- Script mode: alternative syntax that makes it easier to use
  Ginger as a scripting language, used inside {% script %} blocks.

## 0.3.9.1

- Various dependency issues fixed

## 0.3.8.0

- Added a `{% switch %}` statement