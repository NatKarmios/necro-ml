# Necro Ml

At this point the necroml package is not released in the official OPAM repository.

## Installation

### Via `opam`

```
opam switch create necro 4.09.0
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

The `necroml` app produces an OCaml file, which is a module, like all OCaml files.
For instance, when running `necroml myinput.sk -o myinput.ml`, a `myinput.ml` file will be created, which can be referred as a `Myinput` module in another OCaml file.

This module contains a `MakeInterpreter` functor that needs an input module and outputs another module which is the interpreter.
After writing this module, the user can call the functor to create the interpreter :

```ocaml
module Interp = Myinput.MakeInterpreter(I)
```

After that, the user can build terms with the interpreter's constructors, and evaluate them with the specified terms.
Do not hesitate having a look at the examples in the [test](test) folder.

