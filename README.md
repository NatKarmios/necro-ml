# Necro ML

At this point the necroml package is not released in the official OPAM repository.

## Installation

### Via `opam`

```
opam switch create necro 4.09.1
eval $(opam env)
opam repository add necro https://gitlab.inria.fr/skeletons/opam-repository.git#necro
opam install necroml
```

### From the sources

#### Dependencies

* bash
* autoconf
* OCaml (4.09 or greater)
* menhir
* make

To install from the sources, you should first have
[necrolib](https://gitlab.inria.fr/skeletons/necro) installed on your computer,
from the sources also.

Then type `autoconf; ./configure; make`. It should ask for `necrolib`'s path.

## How to run it

There is a single executable file `necroml`. If you used opam, it's in your
path. If you installed from the sources, it is in the root directory.

## How to use it

See [MORE.md](MORE.md) if you want a walk-through on how to use Necro ML.

Do not hesitate to have a look at the examples in the [test](test) folder.

