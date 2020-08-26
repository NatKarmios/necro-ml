open Necro
open Skeltypes

(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)
(* =====          Parameters & useful functions          ===== *)
(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)

let tmp = ref "_tmp"
let env = ref Skeleton.empty_ss

let necro_ocaml_keywords =
	[
	(* OCaml reserved keywords *)
	"and"; "as"; "asr"; "assert"; "begin"; "class"; "constraint"; "do"; "done";
	"downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun";
	"function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer";
	"land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match"; "method"; "mod";
	"module"; "open"; "mutable"; "new"; "nonrec"; "object"; "of"; "open"; "or";
	"private"; "rec"; "sig"; "struct"; "then"; "to"; "true"; "try"; "type"; "val";
	"virtual"; "when"; "while"; "with" ;
	(* ocaml generation reserved keywords *)
	"NotImplemented"
	]

(* print a type in the OCaml way (variable are begun with a single quote)
 * Tries to minimize the number of used parentheses *)
let rec pp_print_type ff (s: necro_type) =
	let rec aux protect ff s =
		let pp_print_type_args ff args =
			match args with
			| [] -> ()
			| [arg] -> Format.fprintf ff "%a " (aux true) arg
			| _ -> Format.fprintf ff "(%a) "
				(Util.print_list ~sep:", " (fun ff s -> aux false ff s)) args
		in
		begin match s with
			| Alias (str, ta, _) ->
					Format.fprintf ff "%a%s" pp_print_type_args ta str
			| Variable s -> Format.fprintf ff "'%s" s
			| Base (name, spec, args) ->
					Format.fprintf ff "%a%s" pp_print_type_args args name
			| Product l ->
					begin match l with
					| [] -> Format.fprintf ff "unit"
					| _ when protect ->
						Format.fprintf ff "(%a)"
						(Util.print_list ~sep:" * " (fun ff s -> aux true ff s)) l
					| _ ->
						Format.fprintf ff "%a"
						(Util.print_list ~sep:" * " (fun ff s -> aux true ff s)) l
					end
			| Arrow (s1, s2) when protect->
					Format.fprintf ff "(%a, %a) M.arrow" (aux true) s1 (aux true) s2
			| Arrow (s1, s2) ->
					Format.fprintf ff "(%a, %a) M.arrow" (aux true) s1 (aux false) s2
		end
	in
	aux false ff s

let rec is_tuple_var p =
	match p with
	| PConstr _ -> false
	| PTuple vl -> List.for_all is_tuple_var vl
	| PVar _ -> true
	| PWild _ -> true

let exhaustive_disjoint env input_type input_terms =
	(* Check that the pattern [p1] is included in pattern [p2], that is, a term which
	 * matches [p1] matches also [p2] *)
	let rec included (p1:pattern) (p2:pattern) =
		match p1, p2 with
		| _, PVar _ -> true
		| _, PWild _ -> true
		| PConstr (c, _, a), PConstr (c', _, a') ->
				c = c' && included a a'
		| PTuple t1, PTuple t2 ->
				List.fold_left2 (fun b p1 p2 -> b && included p1 p2) true t1 t2
		| PVar _, PTuple t -> List.for_all (fun t -> included p1 t) t
		| PWild _, PTuple t -> List.for_all (fun t -> included p1 t) t
		| _, _ -> false
	in
		let rec union_two l1 = function
			| [] -> l1
			| a :: q -> (if List.mem a l1 then Util.id else List.cons a) (union_two l1 q)
		in
	let rec union = function
		| [] -> []
		| a :: [] -> a
		| a :: b :: q -> union ((union_two a b) :: q)
	in
	let match_all = PVar {tv_typ=input_type;tv_name=""} in
	let refined = List.fold_left (fun pl p ->
		union (List.map (Skeleton.common_refinement env input_type p) pl))
		[match_all] input_terms
	in
	let rec aux accu1 value accu2 =
		begin match accu2 with
		| [] when List.exists (fun p -> included p value) accu1 -> accu1
		| [] -> value :: accu1
		| a :: q when List.exists (fun p -> included p value) accu1
		|| List.exists (fun p -> included p value) accu2 ->
				aux accu1 a q
		| a :: q ->
				aux (value :: accu1) a q
		end
	in
	let refined =
		begin match refined with
		| a :: q -> aux [] a q
		| [] -> []
		end
	in
	let exhaustive () =
		List.for_all (fun p -> List.exists (included p) input_terms) refined
	in
	let disjoint () =
		let rec ex2 p =
			begin function
			| [] -> false
			| a :: q when p a -> List.exists p q
			| a :: q -> ex2 p q
			end
		in
		List.for_all (fun p -> not (ex2 (included p) input_terms)) refined
	in
	(exhaustive, disjoint)

let is_exhaustive env input_type input_terms =
	let (x, _) = exhaustive_disjoint env input_type input_terms in
	x ()

let print_loc =
	begin function
	| None -> ""
	| Some ((start_pos, end_pos), file) ->
			let open Lexing in
			let start_col = start_pos.pos_cnum - start_pos.pos_bol + 1 in
			let end_col = end_pos.pos_cnum - start_pos.pos_bol + 1 in
			Printf.sprintf "File %s, line %d, characters %d-%d"
			file start_pos.pos_lnum start_col end_col
	end


let rec print_pattern ff p =
	begin match p with
	| PWild _ -> Format.fprintf ff "_"
	| PVar v -> Format.fprintf ff "%s" v.tv_name
	| PConstr ((c, _), _, PTuple []) -> Format.fprintf ff "%s" c
	| PConstr ((c, _), _, p) -> Format.fprintf ff "%s %a" c print_pattern p
	| PTuple pl -> Format.fprintf ff "(%a)"
		(Util.print_list ~sep:", " print_pattern) pl
	end


let rec print_term ff =
	begin function
	| TVar (x, _) ->
			Format.fprintf ff "%s" x.tv_name
	| TConstr ((c, _), _, TTuple []) ->
			Format.fprintf ff "%s" c
	| TConstr ((c, _), _, t) ->
			Format.fprintf ff "%s %a" c print_term_protect t
	| TTuple tl ->
			Format.fprintf ff "(%a)" (Util.print_list ~sep:", " print_term) tl
	| TFunc (x, _, s) ->
			Format.fprintf ff "@[<v 2>begin function %s ->@,%a@]@,end" x print_skel s
	end

and print_term_protect ff t =
	begin match t with
	| TVar _ | TTuple _ | TConstr (_, _, TTuple []) ->
			print_term ff t
	| _ -> Format.fprintf ff "(%a)" print_term t
	end

and print_bind_skel ff s =
	begin match s with
	| LetIn _ | Return (TFunc _) | Apply (_, _, _ :: _)->
			Format.fprintf ff "@,%a@]@," print_skel s
	| _ -> Format.fprintf ff " %a@] " print_skel s
	end
and print_ret_skel ff s =
	begin match s with
	| LetIn _ | Return (TFunc _) | Apply (_, _, _ :: _) ->
			Format.fprintf ff "@,%a" print_skel s
	| _ -> Format.fprintf ff " %a" print_skel s
	end
and print_skel ff s =
	begin match s with
	| Return t ->
			Format.fprintf ff "M.ret %a" print_term_protect t
	| Apply (f, _, []) ->
			Format.fprintf ff "M.ret %s" f.tv_name
	| Apply (f, _, a :: q) ->
			Format.fprintf ff "let* %s = %s %a in@,%a" !tmp f.tv_name
			print_term_protect a print_skel (Apply ({tv_name= !tmp;tv_typ=f.tv_typ}, [], q))
	| LetIn (None, p, s1, s2) ->
		if is_tuple_var p then
			Format.fprintf ff "@[<v 2>let* %a =%ain%a"
			print_pattern p print_bind_skel s1
			print_ret_skel s2
		else if is_exhaustive !env (Skeleton.pattern_typ p) [p] then
			Format.fprintf ff
			"@[<v 2>let* %s =%ain@,@[<v>begin match %s with@,\
				| @[<v>%a ->\
					%a@]@,\
			end@]"
			!tmp print_bind_skel s1 !tmp print_pattern p print_ret_skel s2
		else
			Format.fprintf ff
			"@[<v 2>begin@,\
				@[<v 2>let* %s =%ain@,\
				match %s with@,\
				| @[<v>%a ->\
					%a@]@,\
				| _ -> M.fail ()@]@,\
				end"
				(* TODO: don't print M.fail when it's useless *)
			!tmp print_bind_skel s1 !tmp print_pattern p print_ret_skel s2
	| LetIn (Some (m, ta), p, s1, s2) ->
			Format.fprintf ff "let* %s = %a in@,let* %s = %s %s in@,%s (function %a ->@,%a)"
			!tmp print_skel s1 !tmp m !tmp !tmp print_pattern p print_skel s2
	| Branching (_, outs, _) ->
				begin match branch_to_match outs with
				| Some txt -> Format.fprintf ff "%t" txt
				| None ->
						Format.fprintf ff "@[<v 2>begin@,@[<v 2>M.branch [@,%a ]@]@]@,end"
							(Format.pp_print_list ~pp_sep:(fun ff () -> Format.fprintf ff " ;@,")
							(fun ff ->
								Format.fprintf ff "@[<v 2>(fun () ->@,%t)@]"
							)) (List.map (fun s ff -> Format.fprintf ff "%a" print_skel s) outs)
				end
	end

and branch_to_match outs =
	let rec aux a s1 =
		begin function
		| [] -> Some []
		| LetIn (a', p, s1', s2) :: q
			when a = a' && s1 = s1' ->
				Option.bind (aux a s1 q) (fun l -> Some ((p, s2) :: l))
		| _ -> None
		end
	in
	begin match outs with
	| [] -> Some (fun ff -> Format.fprintf ff "raise (Branch_fail \"\")")
	| LetIn (a, p, s1, s2) :: q ->
			begin match aux a s1 q with
			| None -> None
			| Some l ->
					let patterns = (p, s2) :: l in
					(* si les motifs sont deux à deux disjoints, on fait un match. Si le
					 * total des motifs ne couvre pas le type du motif, on ajoute un
					 * branch_fail
					 * sinon, on renvoie None
					 * FIXME: gérer aussi le cas a ≠ None *)
					let (exhaustive, disjoint) =
						exhaustive_disjoint !env (Skeleton.pattern_typ p) (List.map fst patterns)
					in
					begin match disjoint (), exhaustive () with
					| true, true ->
						Some (fun ff ->
							Format.fprintf ff "@[<v 0>let* %s = %a in@,begin match %s with@,%a@,end@]"
							!tmp print_skel s1 !tmp
							(Format.pp_print_list (fun ff (pat, skel) ->
								Format.fprintf ff "@[<v 4>| %a ->%a@]" print_pattern pat
							print_ret_skel skel)) patterns)
					| true, false ->
						Some (fun ff ->
							Format.fprintf ff "@[<v 0>let* %s = %a in@,\
							begin match %s with@,%a@,| _ -> M.fail ()@,end@]"
							!tmp print_skel s1 !tmp
							(Format.pp_print_list (fun ff (pat, skel) ->
								Format.fprintf ff "@[<v 4>| %a ->%a@]" print_pattern pat
							print_ret_skel skel)) patterns
							)
					| _ -> None
					end
			end
	| _ -> None
	end




(* Print signature only when necessary (if the term is polymorphic) *)
let print_poly_sig ta (ty: necro_type) ff =
	let ta_print ff ty_print =
		if ta <> [] then
				Format.fprintf ff ": %a. %t" (Util.print_list ~sep:" "
				(fun ff -> Format.fprintf ff "'%s")) ta ty_print
	in
	Format.fprintf ff "%a" ta_print (fun ff -> pp_print_type ff ty)


(* Write the mutually-recursive terms *)
let print_term_definitions ff env =
	let spec_terms =
		env.ss_terms
		|> Util.SMap.bindings
		|> List.filter_map (begin function
			| (name, (_, ta, ty, Some t)) -> Some (name, ta, ty, t)
			| _ -> None
			end)
	in
	let print_term_def ff (name, ta, ty, t) =
		Format.fprintf ff "%s%t =@, %a" name
		(print_poly_sig ta ty) print_term t
	in
	if spec_terms <> [] then
		Format.fprintf ff "\n@,@[<v 2>let rec %a@]"
			(Format.pp_print_list ~pp_sep:(fun ff () -> Format.fprintf ff "@]\n@,@[<v 2>and ")
					print_term_def) spec_terms

(* Prints [type t] for all unspecified types *)
let print_type_decls ff env =
	(* prints the type's name with "type " before it *)
	let print_one_type_declaration ff s =
		begin match s with
		| Product _ | Variable _ | Alias _ | Arrow _ ->
				assert false
		| Base (s, _, []) ->
				Format.fprintf ff "@,type %s" s
		| Base (s, _, _ :: []) ->
				Format.fprintf ff "@,type _ %s" s
		| Base (s, _, args) ->
				let print_one ff _ = Format.fprintf ff "_" in
				Format.fprintf ff "@,type (%a) %s"
				(Util.print_list ~sep:", " print_one) args s
		end
	in
	let ts =
		env.ss_types
		|> Util.SMap.filter (fun _ (_, s,l) -> l = None)
		|> Util.SMap.bindings
		|> List.map (fun (x, (_, y, z)) -> y)
	in
	Util.print_list print_one_type_declaration ff ts


(* if [all], prints [val t: ty] for all terms. If [not all], then prints only the
 * unspecified terms *)
let print_term_signatures ff env =
	(* Prints the signature of a term *)
	let print_term_sig ff name ty =
		Format.fprintf ff "val %s: %a" name pp_print_type ty
	in
	let all_sigs =
		env.ss_terms
		|> Util.SMap.filter (fun a (_, _, _, t) -> t = None)
		|> Util.SMap.bindings
	in
	if all_sigs <> [] then
			Format.fprintf ff "\n@,%a"
			(Format.pp_print_list (fun ff (name, (_, _, ty,_)) ->
				print_term_sig ff name ty)) all_sigs

let print_terms_not_implemented ff env =
	let print_term_not_implemented ff (name, _) =
		Format.fprintf ff "let %s _ = raise (NotImplemented \"%s\")" name name
	in
	let vals =
		env.ss_terms
		|> Util.SMap.filter (fun a (_, _, ty, t) ->
				begin match t, Skeleton.unalias ty with
				| None, Arrow _ -> true | _ -> false
				end)
		|> Util.SMap.bindings
	in
	if vals <> [] then
	Format.fprintf ff "\n@,%a"
		(Format.pp_print_list print_term_not_implemented) vals

let print_specified_types ff ss =
	let print_constructor ff c =
		let print ff t =
			begin match t with
			| Product (_::_) -> Format.fprintf ff "(%a)" pp_print_type t
			| _ -> pp_print_type ff t
			end
		in
		begin match c.cs_input_type with
		| Product [] ->
				Format.fprintf ff "| %s" c.cs_name
		| ty ->
				Format.fprintf ff "| %s of %a" c.cs_name print ty
		end
	in
	let const = Skeleton.get_constructors_by_type ss in
	if const <> [] then
	Format.fprintf ff "\n@,type %a"
		(Format.pp_print_list ~pp_sep:(fun ff () -> Format.fprintf ff "@,and ") (fun
			ff (base_type, const) ->
				Format.fprintf ff "%a =@,%a" pp_print_type base_type
						(Format.pp_print_list print_constructor) const
			)) const

let print_aliases ff ss =
	let empty = Util.SMap.is_empty ss.ss_types in
	begin match empty with
	| true ->
		Format.pp_print_list ~pp_sep:(fun ff () -> Format.fprintf ff "@,") (fun
			ff (name, (_, ta, ty)) ->
				let ta' = List.map (fun a -> Variable a) ta in
				Format.fprintf ff "type %a = %a"
				pp_print_type (Base (name, true, ta')) pp_print_type ty
			) ff (Util.SMap.bindings ss.ss_aliases)
	| false ->
		Format.pp_print_list ~pp_sep:(fun ff () -> ()) (fun
			ff (name, (_, ta, ty)) ->
				let ta' = List.map (fun a -> Variable a) ta in
				Format.fprintf ff "@,and %a = %a"
				pp_print_type (Base (name, true, ta')) pp_print_type ty
			) ff (Util.SMap.bindings ss.ss_aliases)
	end

(* writes the UNSPEC module type *)
let print_unspec_module_type ff =
	Format.fprintf ff "@[<v 2>module type UNSPEC = sig@,%s@,%s%a%a%a@]@,end"
		"module M: MONAD"
		"include TYPES"
		print_specified_types !env
		print_aliases !env
		print_term_signatures !env

(* writes the INTERPRETER module type *)
let print_types_module_type ff =
	Format.fprintf ff "@[<v 2>module type TYPES = sig%a@]@,end"
		print_type_decls !env

let print_monad_module_type ff =
	Format.fprintf ff
{|module type MONAD = sig
  type 'a t
  type ('a, 'b) arrow = 'a -> 'b t
  val ret: 'a -> 'a t
  val branch: (unit, 'a) arrow list -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val fail: unit -> 'a t
end|}

(* writes the MakeInterpreter functor *)
let print_makeinterpreter_functor ff =
	Format.fprintf ff
		 "@[<v 2>module MakeInterpreter (F : UNSPEC) = struct@,include F@,let ( let* ) = M.bind%a@]@,end"
		print_term_definitions !env

let print_unspec_functor ff =
	Format.fprintf ff "@[<v 2>module Unspec (M:MONAD) (T:TYPES) = struct@,%s@,%s@,%s%a%a%a@]@,end@,"
		"exception NotImplemented of string"
		"include T"
		"module M = M"
		print_specified_types !env
		print_aliases !env
		print_terms_not_implemented !env


(* writes the interpreter generator modules to stdout *)
let generate_interpreter sem: string =
	let ok x =
		not (List.mem x necro_ocaml_keywords) &&
		not (Skeleton.all_variables_ss sem x) &&
		not (is_keyword x)
	in
	let () = tmp := Util.fresh ok !tmp in
	let sem = Skeleton.protect_ss necro_ocaml_keywords sem in
	let () = env := sem in
	let ff = Format.str_formatter in
	let () = Format.fprintf ff
		"@[<v>%t\n@,%t\n@,%t\n@,%t\n@,%t@]@."
		print_types_module_type (* module type TYPES *)
		print_monad_module_type (* module type MONAD *)
		print_unspec_module_type (* module type UNSPEC *)
		print_makeinterpreter_functor (* module MakeInterpreter (F: UNSPEC) *)
		print_unspec_functor (* module Unspec (M:MONAD) (T:TYPES) *)
	in Format.flush_str_formatter ()
