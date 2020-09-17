(** * Auxiliary functions **)

(** Wrap a function call to make the whole program fail with the right error code if an exception happens. **)
let wrap f x =
	try f x
	with e ->
		let stack = Printexc.get_backtrace () in
		let msg = Printexc.to_string e in
		Printf.eprintf "Fatal error: %s%s\n" msg stack ;
		exit 1
let wrap2 f x y = wrap (wrap f x) y
let wrap3 f x y z = wrap (wrap2 f x y) z

(* Print usage in stderr and exit *)
let usage () =
	Printf.eprintf
	("Usage: %s [OPTION]... [FILE]...\n\n\
	Get an OCaml interpreter for the last given file. For every file, \
	the dependencies must precede in the list except if the -d option \
	is given.\n\n\
	Options:\n\
	\t-d / --find-deps\tOnly one file must be given. The dependencies are then \
	looked for in the cwd, and in the path of the given file.\n\
	\t-i [FILE]/ --interface [FILE]\tGet the given skeletal interface to be the
	signature for the interpreter\n\
	\t-o [FILE]/ --output [FILE]\tOutputs the result in given file. If this
	option is not provided, the result is printed in stdout.\n")
	Sys.argv.(0) ; exit 1

type minus =
| Depend
| Output of string
| Interface of string

let parse l =
	let rec parse accu_files accu_options l =
		begin match l with
		| [] ->
				(accu_files, accu_options)
		| "-d" :: q | "--find-deps" :: q ->
				parse accu_files (Depend :: accu_options) q
		| "-o" :: [] | "--output" :: [] -> usage ()
		| "-o" :: f :: q | "--output" :: f :: q ->
				parse accu_files (Output f :: accu_options) q
		| "-i" :: [] | "--interface" :: [] -> usage ()
		| "-i" :: f :: q | "--interface" :: f :: q ->
				parse accu_files (Interface f :: accu_options) q
		| a :: q ->
				parse (a :: accu_files) accu_options q
		end
	in parse [] [] (List.tl l)

let get_basename filename =
	filename
	|> Filename.basename
	|> Filename.remove_extension

(** MAIN FUNCTION **)
let () =
	let files, opt = parse (Array.to_list Sys.argv) in
	let f, d =
		begin match files with
		| [] -> usage ()
		| a :: [] when List.mem Depend opt -> a, Necro.find_deps a
		| a :: _ when List.mem Depend opt -> usage ()
		| a :: q -> a, List.rev q
		end
	in
	let output_file =
		let l = List.filter_map (function | Output sk -> Some sk | _ -> None) opt in
		begin match l with
		| [] -> None
		| a :: q -> if List.for_all ((=) a) q then Some a else usage ()
		end
	in
	let ski_file =
		let l = List.filter_map (function | Interface sk -> Some sk | _ -> None) opt in
		begin match l with
		| [] -> None
		| a :: q -> if List.for_all ((=) a) q then Some a else usage ()
		end
	in
	let sem = Necro.parse_and_type d f in
	let ski =
		begin match ski_file with
		| None -> Necro.Skeleton.ski_of_ss sem
		| Some s -> Necro.parse_and_type_interface d s
		end
	in
	let () =
		begin match Skeleton.match_ski_ss ski sem with
		| true -> ()
		| false -> failwith "The interface doesn't match"
		end
	in
	let l = List.map get_basename d in
	let result = wrap3 Generator.generate_interpreter l sem ski in
	Util.print_in_file result output_file

