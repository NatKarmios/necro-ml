open Implicits

module rec T: sig
  type ident = string
  type lit = int
  type vint = int
  type state = (ident, Unspec(Monads.ID)(T).value) Hashtbl.t
end = T

module NewFlow = struct
	include T
	include Unspec(Monads.ID)(T)
  let assert_necro b =
    if not b then M.fail "AssertionError"
  let litToVal lit = Int lit
  let read (ident, state) = Hashtbl.find state ident
  let write (ident, state, value) =
    let () = Hashtbl.replace state ident value in
    state
  let isInt = function
    | Int i -> i
    | _ -> M.fail "Is not an int"
  let isBool = function
    | Bool b -> b
    | _ -> M.fail "Is not a bool"
  let add (a, b) = (a + b)
  let eq (a, b) = if (a = b) then True else False
  let isTrue b = assert_necro(b)
  let isFalse b = assert_necro(not b)
  let false' = false
  let true' = true
  let mkBool b = Bool b
  let mkInt i = Int i
  let one = 1
  let zero = 0
end

module Interp = MakeInterpreter (NewFlow)

open NewFlow
open Interp

let initial_state () = Hashtbl.create 5

let get_var s x = NewFlow.read (x, s)

let () =
  let t =
    Seq (
      Assign ("a", Const 10),
      If (Var "a",
        Assign ("b", Const 2),
        Assign ("b", Const 4))) in
  let s = eval_stmt (initial_state ()) t in
  assert (get_var s "b" = Int 2)

let () =
  let t =
    Plus (Not (Const 42), Equal (Const 18, Const 18)) in
  let s = eval_expr (initial_state ()) t in
  assert (s = Int 1)

let () =
  let t =
    Equal (Const 0, Not (Const 18)) in
  let s = eval_expr (initial_state ()) t in
  assert (s = Bool True)

let () =
  let t =
    Equal (Const 1, Not (Equal (Const 42, Const 18))) in
  let s = eval_expr (initial_state ()) t in
  assert (s = Bool True)


