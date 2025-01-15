# Changelog

## Version 0.3.4
- Update to Necro Lib v0.16.1, but with no includes for now

## Version 0.3.3
- Protect against ocaml keywords
- Fix some issues

## Version 0.3.2
- Protect against ocaml keywords
- Fix some issues

## Version 0.3.1
- Do not use `let rec` for all terms, but instead use it only when necessary
- Fix issue #8
- Fix

## Version 0.3
- Update to use with Necro Lib 0.14.6
- Do not use `let rec` for all terms, but instead use it only when necessary

## Version 0.2.11.4.1
- Fix

## Version 0.2.11.4
- Remove leftover debugging string

## Version 0.2.11.3
- Fix issue #6

## Version 0.2.11.2
- Fix issue #5

## Version 0.2.11.1
- Fix dependencies

## Version 0.2.11
- Update to Necro Lib v0.12 which means
  - application to only one argument
  - add or-patterns

## Version 0.2.10.2
- includes are now printed in the same order everywhere

## Version 0.2.10.1
- Fix

## Version 0.2.10
- Add necromonads to dune

## Version 0.2.9
- Update to Necro Lib 0.11
- add a `--version` option

## Version 0.2.8.1
- Update to Necro Lib 0.10.1

## Version 0.2.8.0.1
- Fix error in dune file

## Version 0.2.8
- Switch to dune

## Version 0.2.7.1
- Fix makefile

## Version 0.2.7
- Update to Necro Lib 0.10 (without switching to dune)

## Version 0.2.6
- Update to Necro Lib 0.9

## Version 0.2.5.3
- Fix BFS monad (acted like ID)

## Version 0.2.5.2
- Update necro-test

## Version 0.2.5.1
- Update to fix in Necro Lib (0.8.4)

## Version 0.2.5
- Update to Necro Lib 0.8
- not generating matches except where there is already one

## Version 0.2.4
- Update to Necro Lib 0.7

## Version 0.2.3
- Remove `kind` from interpretation monad
- Remove `is_exhaustive` and `is_disjoint` (sent to necrolib)

## Version 0.2.2
- renaming "value" to "term" in the AST (and everywhere)

## Version 0.2.1.5
- fix to handle record patterns

## Version 0.2.1.4
- cut the process into two steps, first generate ml, then print. The resulting
  code is somewhat affected, but should be semantically equivalent

## Version 0.2.1.3
- Fix on includes

## Version 0.2.1.2
- add necromonads.cma to opam file
