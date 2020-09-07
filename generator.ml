open Necro
open Skeltypes

(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)
(* =====          Parameters & useful functions          ===== *)
(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)

let tmp = ref "_tmp"
let env = ref Skeleton.empty_ss
type 'a tree = Node of 'a * 'a tree list
let dep_tree: string tree ref = ref (Node ("this", []))
let dep_map = ref (Util.SMap.empty: string list Util.SMap.t)
let all_includes = ref [""]

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

let print_comment ff c =
	Format.fprintf ff "(**%s*)" c

let print_from ff from =
	Util.print_list ~sep:"" (fun ff x ->
		Format.fprintf ff "%s." (String.capitalize_ascii x)) ff from

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
			| Alias (from, str, ta, _) ->
					Format.fprintf ff "%a%a%s" pp_print_type_args ta print_from from str
			| Variable s -> Format.fprintf ff "'%s" s
			| Base (from, name, spec, args) ->
					Format.fprintf ff "%a%a%s" pp_print_type_args args print_from from name
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
	let match_all = PVar ("", input_type) in
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
	| PVar (v, _) -> Format.fprintf ff "%s" v
	| PConstr ((from, c, _), _, PTuple []) -> Format.fprintf ff "%a%s" print_from
	from c
	| PConstr ((from, c, _), _, p) -> Format.fprintf ff "%a%s %a" print_from from c print_pattern p
	| PTuple pl -> Format.fprintf ff "(%a)"
		(Util.print_list ~sep:", " print_pattern) pl
	end


let rec print_term ff =
	begin function
	| TVar (x, _) ->
			Format.fprintf ff "%a%s" print_from x.tv_src x.tv_name
	| TConstr ((from, c, _), _, TTuple []) ->
			Format.fprintf ff "%a%s" print_from from c
	| TConstr ((from, c, _), _, t) ->
			Format.fprintf ff "%a%s %a" print_from from c print_term_protect t
	| TTuple tl ->
			Format.fprintf ff "(%a)" (Util.print_list ~sep:", " print_term) tl
	| TFunc (x, _, s) ->
			Format.fprintf ff "begin function %s ->@,@[<v 2>  %a@]@,end" x print_skel s
	end

and print_term_protect ff t =
	begin match t with
	| TVar _ | TTuple _ | TConstr (_, _, TTuple []) ->
			print_term ff t
	| _ -> Format.fprintf ff "(%a)" print_term t
	end

and print_bind_skel ff s =
	begin match s with
	| LetIn _ | Return (TFunc _) | Apply (_, _, _ :: _ :: _) | Branching _ ->
			Format.fprintf ff "@,%a@]@," print_skel s
	| _ -> Format.fprintf ff " %a@] " print_skel s
	end
and print_ret_skel ff s =
	begin match s with
	| LetIn _ | Return (TFunc _) | Apply (_, _, _ :: _ :: _) | Branching _ ->
			Format.fprintf ff "@,%a" print_skel s
	| _ -> Format.fprintf ff " %a" print_skel s
	end
and print_skel ff s =
	begin match s with
	| Return t ->
			Format.fprintf ff "M.ret %a" print_term_protect t
	| Apply (f, _, []) ->
			Format.fprintf ff "M.ret %a%s" print_from f.tv_src f.tv_name
	| Apply (f, _, a :: []) ->
			Format.fprintf ff "%a%s %a" print_from f.tv_src f.tv_name print_term_protect a
	| Apply (f, _, a :: q) ->
			Format.fprintf ff "let* %s = %a%s %a in@,%a" !tmp print_from f.tv_src f.tv_name
			print_term_protect a print_skel (Apply ({tv_src=[];tv_name= !tmp;tv_typ=f.tv_typ}, [], q))
	| LetIn (None, p, s1, s2) ->
		if is_tuple_var p then
			Format.fprintf ff "@[<v 2>let* %a =%ain@,%a"
			print_pattern p print_bind_skel s1
			print_skel s2
		else if is_exhaustive !env (Skeleton.pattern_typ p) [p] then
			Format.fprintf ff
			"@[<v 2>let* %s =%ain@,@[<v>begin match %s with@,\
				| @[<v>%a ->\
					%a@]@,\
			end@]"
			!tmp print_bind_skel s1 !tmp print_pattern p print_ret_skel s2
		else
			(* FIXME *)
			Format.fprintf ff
			"@[<v 2>let* %s =%ain@,\
				@[<v>begin match %s with@,\
				| @[<v>%a ->%a@]@,\
				| _ -> M.fail ()@,\
				end@]"
			!tmp print_bind_skel s1 !tmp print_pattern p print_ret_skel s2
	| LetIn (Some (from, m, ta), p, s1, s2) ->
			Format.fprintf ff "@[<v 2>let* %s = %ain@,let* %s = %a%s %s in@,%s (function %a ->@,%a)"
			!tmp print_bind_skel s1 !tmp print_from from m !tmp !tmp print_pattern p print_skel s2
	| Branching (_, outs) ->
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
	| [] -> Some (fun ff -> Format.fprintf ff "M.fail ()")
	| LetIn (a, p, s1, s2) :: q ->
			begin match aux a s1 q with
			| None -> None
			| Some l ->
					let patterns = (p, s2) :: l in
					(* si les motifs sont deux à deux disjoints, on fait un match. Si le
					 * total des motifs ne couvre pas le type du motif, on ajoute un
					 * [M.fail ()]
					 * sinon, on renvoie None *)
					let (exhaustive, disjoint) =
						exhaustive_disjoint !env (Skeleton.pattern_typ p) (List.map fst patterns)
					in
					begin match disjoint (), exhaustive () with
					| true, true ->
						Some (fun ff ->
							Format.fprintf ff "@[<v 0>@[<v 2>let* %s =%ain@,begin match %s with@,%a@,end@]"
							!tmp print_bind_skel s1 !tmp
							(Format.pp_print_list (fun ff (pat, skel) ->
								Format.fprintf ff "@[<v 4>| %a ->%a@]" print_pattern pat
							print_ret_skel skel)) patterns)
					| true, false ->
						Some (fun ff ->
							Format.fprintf ff "@[<v 2>let* %s =%ain@,\
							begin match %s with@,%a@,| _ -> M.fail ()@,end"
							!tmp print_bind_skel s1 !tmp
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
			| (name, (c, ta, ty, Some t)) -> Some (name, ta, ty, t, c)
			| _ -> None
			end)
	in
	let print_comm ff c =
		begin match c with
		| None -> ()
		| Some c -> Format.fprintf ff "%a@," print_comment c
		end
	in
	let print_term_def ff (name, ta, ty, t, _) =
		Format.fprintf ff "%s%t =@, %a" name
		(print_poly_sig ta ty) print_term t
	in
	let print_first ff t =
		let (_, _, _, _, c) = t in
		Format.fprintf ff "%a@[<v 2>let rec %a" print_comm c print_term_def t
	in
	let print_not_first ff t =
		let (_, _, _, _, c) = t in
		Format.fprintf ff "@]\n@,%a@[<v 2>and %a" print_comm c print_term_def t
	in
	if spec_terms <> [] then
		Format.fprintf ff "\n@,%a%a@]"
		print_first (List.hd spec_terms)
		(Util.print_list ~sep:"" print_not_first) (List.tl spec_terms)

(* Prints [type t] for all unspecified types *)
let print_type_decls ff env =
	(* prints the type's name with "type " before it *)
	let print_one_type_declaration ff s =
		begin match s with
		| Product _ | Variable _ | Alias _ | Arrow _ ->
				assert false
		| Base (_, s, _, []) ->
				Format.fprintf ff "@,type %s" s
		| Base (_, s, _, _ :: []) ->
				Format.fprintf ff "@,type _ %s" s
		| Base (_, s, _, args) ->
				let print_one ff _ = Format.fprintf ff "_" in
				Format.fprintf ff "@,type (%a) %s"
				(Util.print_list ~sep:", " print_one) args s
		end
	in
	let print_type_with_comment ff (c, s) =
		begin match c with
		| None -> print_one_type_declaration ff s
		| Some c ->
				Format.fprintf ff "@,%a%a" print_comment c print_one_type_declaration s
		end
	in
	let ts =
		env.ss_types
		|> Util.SMap.filter (fun _ (_, s,l) -> l = None)
		|> Util.SMap.bindings
		|> List.map (fun (x, (c, ty, _)) -> (c, ty))
	in
	Util.print_list print_type_with_comment ff ts


(* Prints [val t: ty] for all unspecified terms *)
let print_term_signatures ~all ff env =
	let print_comment ff c =
			begin match c with
			| None -> ()
			| Some c ->
					Format.fprintf ff "%a@," print_comment c
			end
	in
	(* Prints the signature of a term *)
	let print_term_sig ff c name ty =
		Format.fprintf ff "%aval %s: %a" print_comment c name pp_print_type ty
	in
	let all_sigs =
		env.ss_terms
		|> (if all then Util.id else Util.SMap.filter (fun a (_, _, _, t) -> t = None))
		|> Util.SMap.bindings
	in
	if all_sigs <> [] then
			Format.fprintf ff "\n@,%a"
			(Format.pp_print_list (fun ff (name, (c, _, ty,_)) ->
				print_term_sig ff c name ty)) all_sigs

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

(** Prints the specified types. If comm is set to true, then also print the
    special comments *)
let print_specified_types ~comm ff ss =
	let print_constructor ff c =
		let print ff t =
			begin match t with
			| Product (_::_) -> Format.fprintf ff "(%a)" pp_print_type t
			| _ -> pp_print_type ff t
			end
		in
		let print_com ff =
			begin function
			| None -> ()
			| Some c -> Format.fprintf ff " %a" print_comment c
			end
		in
		begin match c.cs_input_type with
		| Product [] ->
				Format.fprintf ff "| %s%a"
				c.cs_name print_com c.cs_comment
		| ty ->
				Format.fprintf ff "| %s of %a%a"
				c.cs_name print ty print_com c.cs_comment
		end
	in
	let print_comment ff c =
		if not comm then () else
			begin match c with
			| None -> ()
			| Some c ->
					Format.fprintf ff "@,%a" print_comment c
			end
	in
	let const = Skeleton.get_constructors_by_type ss in
	let get_comm base_type =
		let name =
			begin match base_type with
			| Base (_, name, _, _) -> name
			| _ -> assert false
			end
		in
		begin match Util.SMap.find_opt name ss.ss_types with
		| Some (c, _, _) -> c
		| None -> failwith "Internal error 1"
		end
	in
	if const <> [] then
		Format.fprintf ff "%a%a"
			(fun ff (ty, c) -> Format.fprintf ff "\n%a@,type %a =@,%a"
				print_comment (get_comm ty) pp_print_type ty
				(Format.pp_print_list print_constructor) c)
			(List.hd const)
				(Util.print_list ~sep:"" (fun ff (ty, c) ->
					Format.fprintf ff "%a@,and %a =@,%a"
					print_comment (get_comm ty) pp_print_type ty
					(Format.pp_print_list print_constructor) c)) (List.tl const)

	(*
	 *if const <> [] then
	 *Format.fprintf ff "\n@,type %a"
	 *  (Format.pp_print_list ~pp_sep:(fun ff () -> Format.fprintf ff "@,and ") (fun
	 *    ff (base_type, const) ->
	 *      Format.fprintf ff "%a =@,%a" pp_print_type base_type
	 *          (Format.pp_print_list print_constructor) const
	 *    )) const
	 *)

let print_aliases ff ss =
	let empty = Util.SMap.is_empty ss.ss_types in
	let print_comment ff c =
			begin match c with
			| None -> ()
			| Some c ->
					Format.fprintf ff "%a@," print_comment c
			end
	in
	begin match empty with
	| true ->
		Format.pp_print_list ~pp_sep:(fun ff () -> Format.fprintf ff "@,") (fun
			ff (name, (c, ta, ty)) ->
				let ta' = List.map (fun a -> Variable a) ta in
				Format.fprintf ff "%atype %a = %a"
				print_comment c
				pp_print_type (Base ([], name, true, ta')) pp_print_type ty
			) ff (Util.SMap.bindings ss.ss_aliases)
	| false ->
		Format.pp_print_list ~pp_sep:(fun ff () -> ()) (fun
			ff (name, (c, ta, ty)) ->
				let ta' = List.map (fun a -> Variable a) ta in
				Format.fprintf ff "@,%aand %a = %a"
				print_comment c
				pp_print_type (Base ([], name, true, ta')) pp_print_type ty
			) ff (Util.SMap.bindings ss.ss_aliases)
	end


(* Function to print "with module _ := _" *)
let print_with ff l =
	let print_one_with ff a =
		let a_cap = String.capitalize_ascii a in
		Format.fprintf ff " with module %s = %s" a_cap a_cap
	in
	Util.print_list print_one_with ff l

let print_includes_unspec print_one ff ss =
	let l = !all_includes in
	Util.print_list ~sep:"" print_one ff l

let print_includes print_one ff ss =
	let Node (_, l) = !dep_tree in
	let includes = List.map (function Node (x, l) -> x) l in
	Util.print_list ~sep:"" print_one ff includes

(* writes the UNSPEC module type *)
let print_unspec_module_type ff =
	let print_one ff i =
		let l = Util.SMap.find i !dep_map in
		let i' = String.capitalize_ascii i in
		Format.fprintf ff "@,module %s:%s.INTERPRETER with module M = M%a"
		i' i' print_with l
	in
	Format.fprintf ff "(** All types, and the unspecified terms *)@,\
		@[<v 2>module type UNSPEC = sig@,%s%a@,%s%a%a%a@]@,end"
		"module M: MONAD"
		(print_includes_unspec print_one) !env
		"include TYPES"
		(print_specified_types ~comm:true) !env
		print_aliases !env
		(print_term_signatures ~all:false) !env

(* writes the TYPES module type *)
let print_types_module_type ff =
	Format.fprintf ff "(** The unspecified types *)@,\
		@[<v 2>module type TYPES = sig%a@]@,end"
		print_type_decls !env

let print_monad_module_type ff =
	Format.fprintf ff
{|(** The interpretation monad *)
module type MONAD = sig
  type 'a t
  type ('a, 'b) arrow = 'a -> 'b t
  val ret: 'a -> 'a t
  val branch: (unit, 'a) arrow list -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val fail: unit -> 'a t
end|}

let print_args ff env =
	let print_include ff (n, sem) =
		Format.fprintf ff "@,(%s:%s.INTERPRETER with module M = F.M)"
		(String.capitalize_ascii n) (String.capitalize_ascii n)
	in
	Util.print_list ~sep:"" print_include ff (Util.SMap.bindings env.ss_includes)

(* writes the MakeInterpreter functor *)
let print_makeinterpreter_functor ff =
	Format.fprintf ff
		"(** Module defining the specified terms *)@,\
		@[<v 2>module MakeInterpreter (F : UNSPEC)%a = struct@,\
		include F@,let ( let* ) = M.bind%a@]@,end"
		print_args !env
		print_term_definitions !env



let print_unspec_functor ff =
	let print_one1 ff i =
		let i' = String.capitalize_ascii i in
		Format.fprintf ff "@,(%s:%s.INTERPRETER with module M = M)"
		i' i'
	in
	let print_one2 ff i =
		let i' = String.capitalize_ascii i in
		Format.fprintf ff "@,module %s = %s"
		i' i'
	in
	Format.fprintf ff
		"(** A default instantiation *)@,\
		@[<v 2>module Unspec (M:MONAD) (T:TYPES)%a = struct@,%s@,%s@,%s%a%a%a%a@]@,end@,"
		(print_includes_unspec print_one1) !env
		"exception NotImplemented of string"
		"include T"
		"module M = M"
		(print_includes_unspec print_one2) !env
		(print_specified_types ~comm:false) !env
		print_aliases !env
		print_terms_not_implemented !env

let print_interpreter_module_type ff =
	let print_one ff i =
		let i' = String.capitalize_ascii i in
		Format.fprintf ff "@,module %s:%s.INTERPRETER with module M = M"
		i' i'
	in
	Format.fprintf ff
	"(** The module type for interpreters *)@,\
	@[<v 2>module type INTERPRETER = sig@,\
	module M:MONAD%a%a%a%a%a@]@,end" 
	(print_includes print_one) !env
	print_type_decls !env
	(print_specified_types ~comm:false) !env
	print_aliases !env
	(print_term_signatures ~all:true) !env

let build_tree includes ss =
	let rec aux (f, e) =
		Node (f, List.map aux (Util.SMap.bindings e.ss_includes))
	in
	let tree = Node (
		"this",
		List.filter_map (fun i ->
			begin match Util.SMap.find_opt i ss.ss_includes with
			| None -> None
			| Some ss -> Some (aux (i, ss))
			end) includes)
	in
	let () = dep_tree := tree in
	()

let build_map includes ss =
	let rec aux map (Node (i, l)) =
		let map = Util.SMap.add i (List.map (function Node (x, _) -> x) l) map in
		List.fold_left aux map l
	in
	let () = dep_map := aux Util.SMap.empty !dep_tree in
	()

(* writes the interpreter generator modules to stdout *)
let generate_interpreter includes sem: string =
	let ok x =
		not (List.mem x necro_ocaml_keywords) &&
		not (Skeleton.all_variables_ss sem x) &&
		not (is_keyword x)
	in
	let () = all_includes := includes in
	let () = build_tree includes sem in
	let () = build_map includes sem in
	let () = tmp := Util.fresh ok !tmp in
	let sem = Skeleton.protect_ss necro_ocaml_keywords sem in
	let () = env := sem in
	let ff = Format.str_formatter in
	let () = Format.fprintf ff
		"%s\n@[<v>%t\n@,%t\n@,%t\n@,%t\n@,%t\n@,%t@]@."
"(** This file was automatically generated using necroml
    See https://gitlab.inria.fr/skeletons/necro-ml/ for more informations *)"
		print_types_module_type (* module type TYPES *)
		print_monad_module_type (* module type MONAD *)
		print_unspec_module_type (* module type UNSPEC *)
		print_makeinterpreter_functor (* module MakeInterpreter (F: UNSPEC) …*)
		print_unspec_functor (* module Unspec (M:MONAD) (T:TYPES) *)
		print_interpreter_module_type (* module type INTERPRETER *)
	in Format.flush_str_formatter ()
