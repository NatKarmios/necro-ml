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
let usage (): 'a =
	Printf.eprintf
	("Usage: %s <file.sk> ... <file.sk> [-d/--find-deps] [-o/--output <file.ml>]\n\
	If no output file is given, the result will be written in stdout.\n")
	Sys.argv.(0);
	exit 1

let parse_options l =
	let rec aux l accu deps =
		begin match l with
		| [] -> (accu, None, deps)
		| ("-o" as s) :: q | ("--output" as s) :: q ->
				begin match q with
				| [] ->
						failwith (s ^ " expects the name of an output file")
				| out :: [] ->
						(accu, Some out, deps)
				| _ :: "-o" :: _ | _ :: "--output" :: _ ->
						failwith "Multiple output files given"
				| _ -> usage ()
				end
		| "-d" :: q | "--find-deps" :: q ->
				if deps then usage () else aux q accu true
		| a :: q -> if deps then usage () else aux q (a :: accu) deps
		end
	in aux l [] false

let get_basename filename =
	filename
	|> Filename.basename
	|> Filename.remove_extension

(** MAIN FUNCTION **)
let () =
	let files, output_file, deps =
		begin match Array.to_list Sys.argv with
		| [] -> assert false
		| _ :: q -> parse_options q
		end
	in
	let files =
		begin match deps, files with
		| true, file :: [] -> Necro.find_deps file
		| _, [] -> usage ()
		| true, _ -> usage ()
		| false, _ -> List.rev files
		end
	in
	let sem = Necro.parse_and_type files in
	let rec get_last =
		begin function
			| [] -> assert false
			| x :: [] -> [], x
			| x :: l -> let l, y = get_last l in x :: l, y
		end 
	in
	let _, x = get_last sem in
	let l, _ = get_last files in
	let l = List.map get_basename l in
	let result = wrap Generator.generate_interpreter l x in
	begin match output_file with
	| None -> print_endline result
	| Some f ->
			let oc = open_out f in
			let () = output_string oc result in
			close_out oc
	end

