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


(* Print usage in stderr and exit *)
let usage () =
	Printf.eprintf
	("Usage: %s [-c/--collect/-r/--random] <file.sk> [-o/--output <file.ml>]\n\
	If no output file is given, the result will be written in stdout.\n")
	Sys.argv.(0);
	exit 1

type semantics = | Collect | Random | Natural

let parse_options l =
	let parse_output l =
		begin match l with
		| [] -> None
		| ("-o" as s) :: q | ("--output" as s) :: q ->
				begin match q with
				| [] ->
						failwith (s ^ " expects the name of an output file")
				| out :: [] ->
						Some out
				| _ :: "-o" :: _ | _ :: "--output" :: _ ->
						failwith "Multiple output files given"
				| _ -> usage ()
				end
		| _ -> usage ()
		end
	in
	begin match l with
	| "-c" :: file :: q | "--collect" :: file :: q ->
			 Generator.generate_interpreter_list, file, parse_output q
	| "-r" :: file :: q | "--random" :: file :: q ->
			Generator.generate_interpreter_rand, file, parse_output q
	| file :: q ->
			Generator.generate_interpreter, file, parse_output q
	| [] -> usage ()
	end



(** MAIN FUNCTION **)
let () =
	let generator, filename, output_file =
		begin match Array.to_list Sys.argv with
		| [] -> assert false
		| _ :: q -> parse_options q
		end
	in
	let sem = Necro.parse_and_type filename in
	let result = wrap generator sem in
	begin match output_file with
	| None -> print_endline result
	| Some f ->
			let oc = open_out f in
			let () = output_string oc result in
			close_out oc
	end

