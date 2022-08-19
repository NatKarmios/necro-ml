# Changelog

## Version 2.8.0.1
- Fix error in dune file

## Version 2.8
- Switch to dune

## Version 2.7.1
- Fix makefile

## Version 2.7
- Update to Necro Lib 0.10 (without switching to dune)

## Version 2.6
- Update to Necro Lib 0.9

## Version 2.5.3
- Fix BFS monad (acted like ID)

## Version 2.5.2
- Update necro-test

## Version 2.5.1
- Update to fix in Necro Lib (0.8.4)

## Version 2.5
- Update to Necro Lib 0.8
- not generating matches except where there is already one

## Version 2.4
- Update to Necro Lib 0.7

## Version 2.3
- Remove `kind` from interpretation monad
- Remove `is_exhaustive` and `is_disjoint` (sent to necrolib)

## Version 2.2
- renaming "value" to "term" in the AST (and everywhere)

## Version 2.1.5
- fix to handle record patterns

## Version 2.1.4
- cut the process into two steps, first generate ml, then print. The resulting
  code is somewhat affected, but should be semantically equivalent

## Version 2.1.3
- Fix on includes

## Version 2.1.2
- add necromonads.cma to opam file
