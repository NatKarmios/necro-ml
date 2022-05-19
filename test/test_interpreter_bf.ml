open Brainfuck

module Input = struct
	include Unspec(Monads.ID)(struct end)
	let bit_of_int = function
		| 0 -> Zero
		| 1 -> One
		| _ -> raise (Invalid_argument "bit_of_int")

	let int_of_bit = function
		| Zero -> 0
		| One -> 1

	let ascii_of_int i =
		let b7, i = bit_of_int (i mod 2), i / 2 in
		let b6, i = bit_of_int (i mod 2), i / 2 in
		let b5, i = bit_of_int (i mod 2), i / 2 in
		let b4, i = bit_of_int (i mod 2), i / 2 in
		let b3, i = bit_of_int (i mod 2), i / 2 in
		let b2, i = bit_of_int (i mod 2), i / 2 in
		let b1, i = bit_of_int (i mod 2), i / 2 in
		Ascii(b1, b2, b3, b4, b5, b6, b7)

	let int_of_ascii = function
		| Ascii (b1, b2, b3, b4, b5, b6, b7) ->
			let i = 0 in
			let i = i * 2 + int_of_bit b1 in
			let i = i * 2 + int_of_bit b2 in
			let i = i * 2 + int_of_bit b3 in
			let i = i * 2 + int_of_bit b4 in
			let i = i * 2 + int_of_bit b5 in
			let i = i * 2 + int_of_bit b6 in
			let i = i * 2 + int_of_bit b7 in
			i

	let input () =
		let c = (read_line ()).[0] in
		let i = int_of_char c in
		ascii_of_int i

	let o = ref ""

	let output a =
		let i = int_of_ascii a in
		let c = char_of_int i in
		o := String.concat "" [!o ; String.make 1 c];
end

module Interp = MakeInterpreter (Input)

open Input
open Interp




(** Parse an AST from a raw bf code **)

let parse str =
	let to_char_list s =
		let ans = ref [] in
		let () = for i = String.length s - 1 downto 0 do
			ans := s.[i] :: !ans
		done in
		!ans
	in
	let str = to_char_list str in
	let rec get_car l = function
		| '.' -> Cons(Out, l)
		| ',' -> Cons(In, l)
		| '+' -> Cons(Plus, l)
		| '-' -> Cons(Minus, l)
		| '>' -> Cons(Greater, l)
		| '<' -> Cons(Less, l)
		| _   -> l
	in
	let rec parse_aux = function
		| [] ->  Nil
		| '[' :: l ->
				let x, l = parse_loop l in
				Cons(Loop x, parse_aux l)
		| ']' :: l -> assert false
		| c :: l -> get_car (parse_aux l) c
	and parse_loop = function
		| [] -> assert false
		| '[' :: l ->
				let x, l' = parse_loop l in
				let x',l'' = parse_loop l' in
				Cons (Loop x, x'), l''
		| ']' :: l -> Nil, l
		| c :: l ->
				let x, l' = parse_loop l in
				get_car x c, l'
	in
	parse_aux str



let exec state code =
	let ((), s) = eval code state in
	s

let () =
	let code = parse "[->+<]>"  in
	let initial_state = State (Nil, ascii_of_int 121, Cons (ascii_of_int 49, Nil)) in
	let State (_, i, _) = exec initial_state code in
	assert(int_of_ascii i = 42)

let () =
	let code = parse "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."  in
	let initial_state = State (Nil, ascii_of_int 0, Nil) in
	let () = o := "" in
	let _ = exec initial_state code in
	assert(!o = "Hello World!\n")






