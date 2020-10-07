(* while language test : needs an interpreter to be generated in "while.ml" *)

open While

module SMap = Map.Make(String)

module rec T: sig
		type ident = string
		type lit = int
		type vint = int
		type state = Unspec(Monads.ID)(T).value SMap.t
		type 'a st = state -> 'a * state
	end = T


module Unspec = struct
	include Unspec(Monads.ID)(T)
  let bind x f =
    function s ->
      let (y, s') = x s in
      f y s'
  let ret x = fun s -> (x, s)

  let litToVal lit = Int lit
  let read ident = fun state -> (SMap.find ident state, state)
  let write (ident, value) = fun state ->
    let new_state = SMap.add ident value state in
    ((), new_state)
  let add (a, b) = a + b
  let eq (a, b) = if a = b then True else False
end

module InterpWhile = MakeInterpreter (Unspec)

open Unspec
open InterpWhile

let initial_state = SMap.empty

let () =
  let t =
    Seq (
      Assign ("a", Const 10),
      If (
        Equal(Var "a", Const 9),
        Assign ("x", Const 1),
        Assign ("x", Const 0)
      )
    )
  in
  let ((), s) = eval_stmt t initial_state in
  assert(fst (Unspec.read "x" s) = Int 0)

let () =
  let t =
    Seq (
      Assign ("n", Const 10),
    Seq (
      Assign ("a", Const 1),
    Seq (
      Assign ("b", Const 1),
    Seq (
      Assign ("c", Const 0),
    Seq (
      Assign ("i", Const 0),
      While (
        Not (Equal (Var "i", Var "n")),
        Seq (
          Assign ("d", Var "b"),
        Seq (
          Assign ("b", Plus (Var "a", Var "b")),
        Seq (
          Assign ("a", Var "d"),
        Seq (
          Assign ("c", Plus (Var "c", Var "b")),
          Assign ("i", Plus (Var "i", Const 1))
        ))))
      )
    )))))
  in
  let ((), s) = eval_stmt t initial_state in
  let n = ref 10 in
  let a = ref 1 in
  let b = ref 1 in
  let c = ref 0 in
  let i = ref 0 in
  let d = ref 0 in
  while (not (i = n)) do
    d := !b ;
    b := !a + !b ;
    a := !d ;
    c := !c + !b ;
    i := !i + 1
  done ;
  assert(fst (Unspec.read "n" s) = Int !n) ;
  assert(fst (Unspec.read "a" s) = Int !a) ;
  assert(fst (Unspec.read "b" s) = Int !b) ;
  assert(fst (Unspec.read "c" s) = Int !c) ;
  assert(fst (Unspec.read "i" s) = Int !i) ;
  assert(fst (Unspec.read "d" s) = Int !d)

let () =
  let t =
    Seq (
      Assign ("over", Const 0),
    Seq (
      Assign ("fact", Const 1),
    Seq (
      Assign ("n", Const 10),
      While (
        Equal(Var "over", Const 0),
        If (
          Equal(Var "n", Const 0),
          Assign ("over", Const 1),
          Seq (
            Assign ("a", Var "fact"),
          Seq (
            Assign ("i", Const 1),
          Seq (
            While (
              Not (Equal(Var "i", Var "n")),
              Seq (
                Assign ("fact", Plus (Var "fact", Var "a")),
                Assign ("i", Plus (Var "i", Const 1))
              )
            ),
            Assign ("n", Plus (Var "n", Const (-1)))
          )))
        )
      )
    )))
  in
  let ((), s) = eval_stmt t initial_state in
  let rec fact = function
    | 0 -> 1
    | i -> i * fact (i - 1) in
  assert(fst (Unspec.read "fact" s) = Int (fact 10))

