(* while language test : needs an interpreter to be generated in "while.ml" *)

open While_amb

module SMap = Map.Make(String)

module T = struct
  type ident = string
  type lit = int
  type vint = int
  type vbool = bool
  type value =
  | Bool of vbool
  | Int of vint
  type state = value SMap.t
end

module Flow = struct
	include T
	include Flow(T)
  module S = struct
    type 'a t = state -> 'a * state
    let bind x f =
      function s ->
        let (y, s') = x s in
        f y s'
    let ret x = fun s -> (x, s)
  end

  let add (a, b) = [Int (a + b)]
  let eq (a, b) = [Bool (a = b)]
  let isBool = function
    | Bool b -> [b]
    | _ -> []
  let isFalse = function
    | true -> []
    | false -> [()]
  let isInt = function
    | Int i -> [i]
    | _ -> []
  let isTrue = function
    | true -> [()]
    | false -> []
  let litToVal lit = [Int lit]
  let neg = function
    | true -> [Bool false]
    | false -> [Bool true]
  let read (ident, state) =
    Option.to_list (SMap.find_opt ident state)
  let write (ident, state, value) =
    [SMap.add ident value state]
end

module InterpWhile = MakeInterpreter (Flow)

open T
open Flow
open InterpWhile

let initial_state = SMap.empty

let rec has_values l1 l2 = match l1 with
  | [] -> l2 = []
  | a :: q ->
      List.mem a l2 &&
      has_values q (List.filter (fun x -> x <> a) l2)

let () =
  let two_or_three = Amb (Cons (Const 2, Cons(Const 3, Nil))) in
  let t =
    Seq (
      Assign ("a", two_or_three),
      Assign ("b", Plus (Var "a", Var "a"))
    )
  in
  let s = stmt (initial_state, t) in
  let b = List.map (SMap.find_opt "b") s in
  assert(has_values [Some (Int 4); Some (Int 6)] b)

