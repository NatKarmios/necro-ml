# Installation

To use necroml, it is mandatory to have installed necrostd first. You may
install it directly from the [source](https://gitlab.inria.fr/skeletons/necro)
or using `opam`

Then you need to update the submodules (if you didn't clone recursively) :
```bash
git submodule update --init
```

## Install with `opam`

You will need `autoconf` installed

```bash
opam switch create necro 4.09.1
opam repository add necro https://gitlab.inria.fr/skeletons/opam-repository.git#necro
opam install necrolib
eval $(opam env)
```
