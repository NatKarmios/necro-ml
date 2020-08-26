open Lists

(* Natural semantics *)
	module Interp = MakeInterpreter(Unspec(Monads.ID)(struct end))
	open Interp

	let rec of_list = function
		| [] -> Nil
		| a :: q -> Cons (a, of_list q)

	let rec of_bool = function
		| true -> True
		| false -> False

	let () =
		let a = of_list [8; 3; 4; 1; 9; 2; 7; 5; 6] in
		let b = of_list [of_list [1; 2; 3]; of_list [4; 5; 6]; of_list [7; 8; 9]] in
		let le x y = of_bool (x < y) in
		assert (flatten b = M.bind (sort le) (fun g -> g a))

(* Collecting semantics *)
	module Interp2 = MakeInterpreter(Unspec(Monads.List)(struct end))
	open Interp2

	let rec of_list = function
		| [] -> Nil
		| a :: q -> Cons (a, of_list q)

	let rec of_bool = function
		| true -> True
		| false -> False

	let rec has_values l1 l2 = match l1 with
		| [] -> l2 = []
		| a :: q ->
				List.mem a l2 &&
				has_values q (List.filter (fun x -> x <> a) l2)

	let () =
		let div2 x = M.ret (of_bool (x mod 2 == 0)) in
		let a = of_list [1; 2; 3; 4; 5; 6; 7; 8; 9] in
		let l =
			let* tmp = find div2 in
			tmp a
		in
		assert (has_values [2;4;6;8] l)
