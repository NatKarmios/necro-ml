# How to use Necro ML (Walkthrough)

The `necroml` app produces an OCaml file, which is a module, like all OCaml files.
For instance, when running `necroml myinput.sk -o myinput.ml`, a `myinput.ml` file will be created, which can be referred as a `Myinput` module in another OCaml file.

Let us analyze the content of `myinput.ml` and explain what needs to be done to
create an interpreter.

This module contains several pieces.

## `module type TYPES`

The module type `TYPES` contains the non-specified types. In the interpreter
file, you need to instantiate it by creating a module whose signature
includes that of `TYPES`

## `module type MONAD`

The module type `MONAD` contains the necessary value for the interpretation
monad, that is the way that the skeletons (for instance branches) are evaluated
in your interpreter. You can check `necromonads.ml` which provides several
standard choices. The `cma` version of this file is available with opam
installation of Necro ML

## `module type UNSPEC`

The module type `UNSPEC` provides the definition for the specified types, and
requires a definition for the non-specified types and values.

A default instantiation of it is provided as `module Unspec` later in the file.
`Unspec` is a functor that requires that you choose the monad and the
non-specified types. It then generates a module.

So the usual way to define a module of type `UNSPEC` is that way:

```ocaml
module Input = struct
	include Unspec(Monads.ID)(Types)
	(* Definition of non-specified values
	…
	*)
end
```

## `module MakeInterpreter (F: UNSPEC)` and `module type INTEPRETER`

Once you have created your module of type `UNSPEC`, you can provide it to the
`MakeInterpreter` functor, to create an interpreter. Its signature will match
that of `INTERPRETER`.
