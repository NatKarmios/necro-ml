open Sudoku

module Input(M:MONAD) = struct
	include Unspec(M)(struct end)
	(Interpreter_boolean.Interp(M))(Interpreter_nat.Interp(M))(Interpreter_list.Interp(M))
end

module Interp(M:MONAD) =
	MakeInterpreter(Input(M))(Interpreter_boolean.Interp(M))(Interpreter_list.Interp(M))(Interpreter_nat.Interp(M))


let parse file =
	let ic = open_in file in
	let rec f acc =
		try f ((input_line ic) :: acc)
		with End_of_file -> acc
	in
	let rec int_sqrt n start =
		let start2 = start * start in
		if start2 = n then Some start
		else if start2 > n then None
		else int_sqrt n (start + 1)
	in
	let lines = f [] in
	let () = close_in ic in
	let size2 = Stdlib.List.length lines in
	let size =
		begin match int_sqrt size2 1 with
		| None -> failwith "The number of lines in the sudoku is not a square, that is incorrect."
		| Some s -> s
		end
	in
	let parse_char i =
		begin match i with
		| '-' | ' ' | '0' -> 0
		| '1' -> 1 | '2' -> 2
		| '3' -> 3 | '4' -> 4
		| '5' -> 5 | '6' -> 6
		| '7' -> 7 | '8' -> 8
		| '9' -> 9 | _ -> failwith ("Unknown character "^String.make 1 i)
		end
	in
	let parse_line line =
		let () = if String.length line <> size2 then
			failwith "The sudoku is not a square." in
		Stdlib.List.map (fun i ->
			parse_char line.[i]) (Stdlib.List.init size2 (fun x -> x))
	in
	let grid = Stdlib.List.rev_map parse_line lines in
	(size, grid)


let sudoku =
	parse (Sys.argv.(1))

module Tester(M:MONAD) = struct
	open Interp(M)
	open Nat
	open List

	let rec nat_to_int =
		begin function
		| Z -> 0
		| S i -> 1 + nat_to_int i
		end

	let rec int_to_nat =
		begin function
		| 0 -> Z
		| n -> S (int_to_nat (n - 1))
		end

	let rec of_caml_list =
		begin function
		| [] -> Nil
		| a :: q -> Cons (a, of_caml_list q)
		end

	let of_grid g =
		of_caml_list (Stdlib.List.map 
		(fun l -> of_caml_list (Stdlib.List.map int_to_nat l)) g)

	let print_nat n =
		begin match n with
		| Z -> " "
		| _ -> string_of_int (nat_to_int n)
		end

	let print_line_sep size =
		String.make (1+size*(1+size)) '-' ^ "\n"

	let print_line size line =
		let rec print_line_aux remains line accu =
			begin match remains, line with
			| 0, _ -> print_line_aux size line (accu ^ "|")
			| _, Cons(a, q) -> print_line_aux (remains-1) q (accu ^ print_nat a)
			| _, Nil -> accu
			end
		in
		print_line_aux 0 line "" ^ "\n"

	let print_grid (size, grid) =
		let size = nat_to_int size in
		let rec print_grid_aux remains line accu =
			begin match remains, line with
			| 0, _ -> print_grid_aux size line (accu ^ print_line_sep size)
			| _, Cons(a, q) -> print_grid_aux (remains-1) q (accu ^ print_line size a)
			| _, Nil -> accu
			end
		in
		print_grid_aux 0 grid ""

	let solve g =
		ignore (M.extract (solve g))

	let cast sudoku =
		let (size, grid) = sudoku in
		(int_to_nat size, of_grid grid)

	let _ =
		let t = Sys.time () in
		try
			solve (cast sudoku) ;
			let time = Sys.time () -. t in
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
	print_endline "BFS monad (with optimization):";
	let open Tester(Monads.BfsOptim) in
	()



