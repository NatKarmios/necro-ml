open Concurrent

module SMap = Map.Make(String)

module T = struct
  type ident = string
  type lit = int
  type vint = int
  type vbool = bool
  type value =
    | Int of vint
    | Bool of vbool
  type state = value SMap.t
end

module Unspec = struct
	include T
	include Unspec(Monads.List)(T)
  let litToVal lit = [Int lit]
  let read (ident, state) = Option.to_list (SMap.find_opt ident state)
  let write (ident, state, value) =
    [SMap.add ident value state]
  let add a = [function b -> [Int (a + b)]]
  let eq a = [function b -> [Bool (a = b)]]
  let isBool = function
    | Bool b -> [b]
    | _ -> []
  let isInt = function
    | Int i -> [i]
    | _ -> []
  let isFalse b = if b then [] else [()]
  let isTrue b = if b then [()] else []
  let neg b = [Bool (not b)]
end

module InterpWhile = MakeInterpreter (Unspec)

open T
open Unspec
open InterpWhile

let ( let* ) l f = List.flatten (List.map f l)
let rec has_values l1 l2 = match l1 with
  | [] -> l2 = []
  | a :: q ->
      List.mem a l2 &&
      has_values q (List.filter (fun x -> x <> a) l2)

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
  let s =
    let* s1 = eval_stmt (SMap.empty) in
    s1 t
  in
  let x = List.map (SMap.find_opt "x") s in
  assert(has_values [Some (Int 0)] x)

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
  let fact =
    let rec aux accu = function
      | 0 -> accu
      | n -> aux (n * accu) (n - 1)
    in
    aux 1
  in
  let s =
    let* s1 = eval_stmt (SMap.empty) in
    s1 t
  in
  let x = List.map (SMap.find_opt "fact") s in
  assert(has_values [Some (Int (fact 10))] x)

let () =
  let t =
    Parallel (
      Assign ("shared", Const 1),
      Assign ("shared", Const 2)
    )
  in
  let s =
    let* s1 = eval_stmt (SMap.empty) in
    s1 t
  in
  let x = List.map (SMap.find_opt "shared") s in
  assert(has_values [Some (Int 2);Some (Int 1)] x)

