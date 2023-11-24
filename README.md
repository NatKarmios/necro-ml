# Necro ML

At this point the necroml package is not released in the official OPAM repository.

## Installation

### Via `opam`

```
opam switch create necro 4.14.1
eval $(opam env)
opam repository add necro https://gitlab.inria.fr/skeletons/opam-repository.git#necro
opam install necroml
```

## How to run it

There is a single executable file `necroml`. If you used opam, it's in your
path. If you installed from the sources, it is in the root directory.

There is also a file with interpretation monads call necromonads.cma

## How to use it

See [MORE.md](MORE.md) if you want a walk-through on how to use Necro ML. Here is a quick description for setting up a dune project:
- `dune init project foo`
- create `foo.sk` in `foo/bin`
- create a `foo/bin/dune` file with
```
(executable
 (public_name foo)
 (name main)
 (libraries necroml))

(rule
  (target skfoo.ml)
  (deps foo.sk)
  (action (run necroml -o %{target} %{deps})))
```
- the instantiation is done in `foo/bin/main.ml` and can be run doing `dune exec foo`


Do not hesitate to have a look at the examples in the [test](test) folder.

