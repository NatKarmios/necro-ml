(* while language test : needs an interpreter to be generated in "while.ml" *)

open While_amb

module SMap = Map.Make(String)

module Unspec(M: MONAD) = struct
	module rec T : sig
		type ident = string
		type lit = int
		type vint = int
		type state = Unspec(M)(T).value SMap.t
	end = T
	include T

	include Unspec(M)(T)
  let add (a, b) = M.ret (a + b)
  let eq (a, b) = M.ret (if a = b then True else False)
  let litToVal lit = M.ret (Int lit)
  let read (ident, state): value M.t =
		begin match SMap.find_opt ident state with
		| None -> M.fail ("unbound value " ^ ident)
		| Some x -> M.ret x
		end
  let write (ident, state, value) =
    M.ret (SMap.add ident value state)
end

let initial_state = SMap.empty

(* Test something deterministic *)
let () =
	let open MakeInterpreter(Unspec(Monads.ID)) in
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
	let s = eval_stmt (initial_state, t) in
	assert(read ("x", s) = Int 0)

let rec has_values l1 l2 = match l1 with
	| [] -> l2 = []
	| a :: q ->
			List.mem a l2 &&
			has_values q (List.filter (fun x -> x <> a) l2)

(* Test something non-deterministic *)
let () =
	let open MakeInterpreter(Unspec(Monads.List)) in
	let two_or_three = Amb (Cons (Const 2, Cons(Const 3, Nil))) in
	let t =
		Seq (
			Assign ("a", two_or_three),
			Assign ("b", Plus (Var "a", Var "a"))
		)
	in
	let s = eval_stmt (initial_state, t) in
	let b = List.map (SMap.find_opt "b") s in
	assert(has_values [Some (Int 4); Some (Int 6)] b)

