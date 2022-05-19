open Buggy_branch

module Input = Unspec(Monads.ContPoly)(struct end)

module Interp = MakeInterpreter(Input)

open Input
open Interp

let () = Monads.ContPoly.extract (test ())
