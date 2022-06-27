(* while language test : needs an interpreter to be generated in "while.ml" *)

open While

module SMap = Map.Make(String)

module Input(M:MONAD) = struct
	module rec T: sig
			type ident = string
			type lit = int
			type vint = int
			type state = Unspec(M)(T).value SMap.t
		end = T

	include Unspec(M)(T)
	let litToVal lit = M.ret (Int lit)
	let read (ident, state) = M.ret (SMap.find ident state)
	let write (ident, state, value) =
		let new_state = SMap.add ident value state in
			M.ret (new_state)
	let add (a, b) = M.ret (a + b)
	let eq (a, b) = M.ret (if a = b then True else False)
end


module Tester(M:MONAD) = struct
	open MakeInterpreter(Input(M))

	let initial_state = SMap.empty

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
		let start = Sys.time () in
		try
			for i = 1 to 9999 do
				ignore(M.extract(eval_stmt (initial_state, t)))
			done;
			let s = M.extract(eval_stmt (initial_state, t)) in
			let rec fact = function
				| 0 -> 1
				| i -> i * fact (i - 1) in
			assert(SMap.find_opt "fact" s = Some (Int (fact 10))) ;
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

