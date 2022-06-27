(* while language test : needs an interpreter to be generated in "while.ml" *)

open Concurrent_ss

module SMap = Map.Make(String)

module T = struct
		type ident = string
		type lit = int
		type vint = int
		type vbool = bool
		type value = | Int of vint | Bool of vbool
		type state = value SMap.t
	end


module Input(M:MONAD) = struct
	include Unspec(M)(T)
	open T
	let litToVal lit = M.ret (Int lit)
	let read (ident, state) = M.ret (SMap.find ident state)
	let write (ident, state, value) =
		let new_state = SMap.add ident value state in
			M.ret (new_state)
	let add (a, b) = M.ret (Int (a + b))
	let eq (a, b) = M.ret (Bool (a = b))
	let isInt =
		begin function
		| Int i -> M.ret i
		| _ -> M.fail "Not an int"
		end
	let isBool =
		begin function
		| Bool i -> M.ret i
		| _ -> M.fail "Not a bool"
		end
	let isTrue b =
		if b then M.ret () else M.fail "Not true"
	let isFalse b =
		if b then M.fail "Not false" else M.ret ()
	let neg b = M.ret (Bool (not b))
end

module Tester(M:MONAD) = struct
	open MakeInterpreter(Input(M))

	let initial_state = SMap.empty

	let () =
		(*
			count := 0
			y := 0
			while count ≠ 10 do
				x := 0 // x := 1
				y := y + x
				count := count + 1
			done
			assert ( y = 8 )
		*)
		let t =
			Seq(
				Seq(Assign ("x", Const 2), 
				While (Not (Equal (Var "x", Const 0)), Assign ("x", Plus (Var "x", Const
				(-1))))),
			Seq (
				Seq (Seq (Seq (
					Parallel (
						Assign ("x", Const 0),
						Assign ("x", Const 1)),
					Parallel (
						Assign ("y", Const 0),
						Assign ("y", Const 1))),
					Parallel (
						Assign ("z", Const 0),
						Assign ("z", Const 1))),
					Assign ("w", Plus (Var "x", Plus (Var "y", Var "z")))),
				Assert (Equal (Var "w", Const 2))
			))
		in
		let start = Sys.time () in
		try
			ignore (M.extract (apply2 eval_stmt initial_state t));
			let time = Sys.time () -. start in
			print_float time; print_string "s\n\n"
		with _ ->
			print_endline "This monad failed\n"
end

let _ =
	print_endline "Identity monad:";
	let open Tester(Monads.ID) in
	print_endline "List monad:";
	let open Tester(Monads.List) in
	print_endline "Continuation monad:";
	let open Tester(Monads.ContPoly) in
	print_endline "BFS monad:";
	let open Tester(Monads.Bfs) in
	()

