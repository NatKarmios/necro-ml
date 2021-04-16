open Necro
open Skeltypes


(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)
(* =====                Forget locations                 ===== *)
(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)

type value' =
	| TVar' of typed_var
	| TConstr' of constructor * necro_type list * value'
	| TTuple' of value' list
	| TFunc' of string * necro_type * skeleton'
	| TField' of value' * necro_type list * constructor
	| TRecord' of value' option * (constructor * necro_type list * value') list

and skeleton' =
	| Branching' of necro_type * skeleton' list
	| LetIn' of (from * string * necro_type list) option * pattern * skeleton' * skeleton'
	| Return' of value'
	| Apply' of value' * value' list

let interpretation' =
	let open Interpretation in {
		var_interp' = (fun a -> TVar' a) ;
		constr_interp' = (fun a b c -> TConstr' (a, b, c)) ;
		tuple_interp' = (fun a -> TTuple' a) ;
		function_interp' = (fun a b c -> TFunc' (a, b, c)) ;
		field_interp' = (fun a b c -> TField' (a, b, c)) ;
		record_interp' = (fun a b -> TRecord' (a, b)) ;
		letin_interp' = (fun a b c d ->
			begin match a with
			| None -> LetIn' (None, b, c, d)
			| Some (x, y, z, _) -> LetIn' (Some (x, y.contents, z), b, c, d)
			end) ;
		apply_interp' = (fun a b -> Apply' (a, b)) ;
		merge_interp' = (fun a b -> Branching' (a, b)) ;
		return_interp' = (fun a -> Return' a) ;
		exists_interp' = (fun _ _ _ ->
			failwith "Existentials are not supported by Necro ML yet")
	}

let (value'_of_value, skel'_of_skel) =
	let open Interpretation in
	(interpret_value' interpretation', interpret_skeleton' interpretation')


(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)
(* =====          Parameters & useful functions          ===== *)
(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)

let tmp = ref "_tmp"
type 'a tree = Node of 'a * 'a tree list
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
					Format.fprintf ff "(%a -> %a M.t)" (aux true) s1 (aux true) s2
			| Arrow (s1, s2) ->
					Format.fprintf ff "(%a -> %a M.t)" (aux true) s1 (aux true) s2
		end
	in
	aux false ff s

(* FIXME: rename. [is_tuple_var] checks whether the pattern is a valid OCaml
	pattern, or whether it would raise a warning *)
let rec is_tuple_var p =
	match p with
	| PConstr _ -> false
	| PTuple vl -> List.for_all is_tuple_var vl
	| PVar _ -> true
	| PWild _ -> true
	| PRecord _ -> true

let exhaustive_disjoint sem input_type input_values =
	(* Check that the pattern [p1] is included in pattern [p2], that is, a value which
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
		union (List.map (Skeleton.common_refinement sem input_type p) pl))
		[match_all] input_values
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
		List.for_all (fun p -> List.exists (included p) input_values) refined
	in
	let disjoint () =
		let rec ex2 p =
			begin function
			| [] -> false
			| a :: q when p a -> List.exists p q
			| a :: q -> ex2 p q
			end
		in
		List.for_all (fun p -> not (ex2 (included p) input_values)) refined
	in
	(exhaustive, disjoint)

let is_exhaustive sem input_type input_values =
	let (x, _) = exhaustive_disjoint sem input_type input_values in
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


let rec get_from_ski from ski =
	begin match from with
	| [] -> ski
	| a :: q -> get_from_ski q (Util.SMap.find a ski.ski_includes).contents
	end

let rec print_pattern sem ff p =
	let print_record_field ff ((from, c, _), _, p) =
		Format.fprintf ff "%a%s = %a" print_from from c (print_pattern sem) p
	in
	begin match p with
	| PWild _ -> Format.fprintf ff "_"
	| PVar (v, _) -> Format.fprintf ff "%s" v
	| PConstr ((from, c, ty), _, PTuple []) ->
			let f, ty =
				begin match Skeleton.unalias ty with
				| Base (f, ty, _, _) -> f, ty
				| _ -> assert false
				end
			in
			let ski = get_from_ski f (Skeleton.ski_of_ss sem) in
			let (_, _, cl) = Util.SMap.find ty ski.ski_types in
			let cdef = List.find (fun cs -> cs.s_name = c) (Option.get cl) in
			let input = cdef.s_input_type in
			if input = Product [] then
				Format.fprintf ff "%a%s" print_from from c
			else
				Format.fprintf ff "%a%s ()" print_from from c
	| PConstr ((from, c, _), _, p) -> Format.fprintf ff "%a%s %a" print_from from
		c (print_pattern sem) p
	| PTuple pl -> Format.fprintf ff "(%a)"
		(Util.print_list ~sep:", " (print_pattern sem)) pl
	| PRecord pl -> Format.fprintf ff "{%a}"
		(Util.print_list ~sep:"; " print_record_field) pl
	end

let rec print_value sem ff =
	let print_record_field ff ((from, c, _), _, t) =
		Format.fprintf ff "%a%s = %a" print_from from c (print_value sem) t
	in
	begin function
	| TVar' (TVLet (n, _)) -> Format.fprintf ff "%s" n
	| TVar' (TVValue (f, n, _, _)) ->
			Format.fprintf ff "%a%s" print_from f n
	| TConstr' ((from, c, ty), _, TTuple' []) ->
			let f, ty =
				begin match Skeleton.unalias ty with
				| Base (f, ty, _, _) -> f, ty
				| _ -> assert false
				end
			in
			let ski = get_from_ski f (Skeleton.ski_of_ss sem) in
			let (_, _, cl) = Util.SMap.find ty ski.ski_types in
			let cdef = List.find (fun cs -> cs.s_name = c) (Option.get cl) in
			let input = cdef.s_input_type in
			if input = Product [] then
				Format.fprintf ff "%a%s" print_from from c
			else
				Format.fprintf ff "%a%s ()" print_from from c
	| TConstr' ((from, c, _), _, t) ->
			Format.fprintf ff "%a%s %a" print_from from c (print_value_protect sem) t
	| TTuple' tl ->
			Format.fprintf ff "(%a)" (Util.print_list ~sep:", " (print_value sem)) tl
	| TFunc' (x, _, s) ->
			Format.fprintf ff "begin function %s ->@,@[<v 2>  %a@]@,end" x
			(print_skel sem) s
	| TField' (t, ta, (from, f, _)) ->
			begin match int_of_string_opt f with
			| None -> Format.fprintf ff "%a.%a%s" (print_value sem) t print_from from f
			| Some 1 when List.length ta = 2 ->
					Format.fprintf ff "fst %a" (print_value_protect sem) t
			| Some 2 when List.length ta = 2 ->
					Format.fprintf ff "snd %a" (print_value_protect sem) t
			| Some i ->
					let l =
						List.init (i-1) (fun _ -> "_") @
						"x" :: List.init (List.length ta - i) (fun _ -> "_")
					in
					let pattern = String.concat ", " l in
					Format.fprintf ff "let (%s) = %a in x" pattern (print_value sem) t
			end
	| TRecord' (None, tl) ->
			Format.fprintf ff "{%a}" (Util.print_list ~sep:"; " print_record_field) tl
	| TRecord' (Some t, tl) ->
			Format.fprintf ff "{%a with %a}"
			(print_value sem) t (Util.print_list ~sep:"; " print_record_field) tl
	end

and print_value_protect sem ff t =
	begin match t with
	| TVar' _ | TTuple' _ | TConstr' (_, _, TTuple' []) ->
			print_value sem ff t
	| _ -> Format.fprintf ff "(%a)" (print_value sem) t
	end

and print_bind_skel sem ff s =
	begin match s with
	| LetIn' _ | Return' (TFunc' _)
	| Apply' (_, _ :: _ :: _) | Branching' _ ->
			Format.fprintf ff "@,%a@]@," (print_skel sem) s
	| _ -> Format.fprintf ff " %a@] " (print_skel sem) s
	end
and print_bind_value sem ff s =
	begin match s with
	| TFunc' _ -> assert false
	| _ -> Format.fprintf ff " %a@] " (print_value sem) s
	end
and print_ret_skel sem ff s =
	begin match s with
	| LetIn' _ | Return' (TFunc' _)
	| Apply' (_, _ :: _ :: _) | Branching' _ ->
			Format.fprintf ff "@,%a" (print_skel sem) s
	| _ -> Format.fprintf ff " %a" (print_skel sem) s
	end
and print_skel sem ff s =
	begin match s with
	| Return' t ->
			Format.fprintf ff "M.ret %a" (print_value_protect sem) t
	| Apply' (t, []) ->
			Format.fprintf ff "M.ret %a" (print_value_protect sem) t
	| Apply' (f, q) ->
			Format.fprintf ff "apply%d %a %a" (List.length q)
			(print_value_protect sem) f
			(Util.print_list ~sep:" " (print_value_protect sem)) q
	| LetIn' (None, p, s1, s2) ->
		if is_tuple_var p then
			Format.fprintf ff "@[<v 2>let* %a =%ain@,%a"
			(print_pattern sem) p (print_bind_skel sem) s1
			(print_skel sem) s2
		else if is_exhaustive sem (Skeleton.pattern_typ p) [p] then
			begin match s1 with
			| Return' t ->
				Format.fprintf ff
				"@[<v>@[<v 2>begin match%awith@,\
					| @[<v>%a ->\
						%a@]@,\
				end@]"
				(print_bind_value sem) t (print_pattern sem) p (print_ret_skel sem) s2
			| _ ->
				Format.fprintf ff
				"@[<v 2>let* %s =%ain@,@[<v>begin match %s with@,\
					| @[<v>%a ->\
						%a@]@,\
				end@]"
				!tmp (print_bind_skel sem) s1 !tmp (print_pattern sem) p (print_ret_skel sem) s2
			end
		else
			(* FIXME *)
			begin match s1 with
			| Return' t ->
			Format.fprintf ff
			"@[<v>@[<v 2>begin match %a with@,\
				| @[<v>%a ->%a@]@,\
				| _ -> M.fail \"\"@,\
				end@]"
			(print_bind_value sem) t (print_pattern sem) p (print_ret_skel sem) s2
			| _ ->
			Format.fprintf ff
			"@[<v 2>let* %s =%ain@,\
				@[<v>begin match %s with@,\
				| @[<v>%a ->%a@]@,\
				| _ -> M.fail \"\"@,\
				end@]"
			!tmp (print_bind_skel sem) s1 !tmp (print_pattern sem) p (print_ret_skel sem) s2
			end
	| LetIn' (Some (from, m, ta), p, s1, s2) ->
			Format.fprintf ff "@[<v 2>let* %s = %ain@,apply2 %a%s %s (function %a ->@,%a)"
			!tmp (print_bind_skel sem) s1 print_from from m !tmp
			(print_pattern sem) p (print_skel sem) s2
	| Branching' (_, outs) ->
				begin match branch_to_match sem outs with
				| Some txt -> Format.fprintf ff "%t" txt
				| None ->
						Format.fprintf ff "@[<v 2>begin@,@[<v 2>M.branch [@,%a ]@]@]@,end"
							(Format.pp_print_list ~pp_sep:(fun ff () -> Format.fprintf ff " ;@,")
							(fun ff ->
								Format.fprintf ff "@[<v 2>(fun () ->@,%t)@]"
							)) (List.map (fun s ff -> Format.fprintf ff "%a" (print_skel sem) s) outs)
				end
	end

and branch_to_match sem outs =
	let rec aux a s1 =
		begin function
		| [] -> Some []
		| LetIn' (a', p, s1', s2) :: q
			when a = a' && s1 = s1' ->
				Option.bind (aux a s1 q) (fun l -> Some ((p, s2) :: l))
		| _ -> None
		end
	in
	begin match outs with
	| [] -> Some (fun ff -> Format.fprintf ff "M.fail \"\"")
	| LetIn' (a, p, s1, s2) :: q when a = None ->
			begin match aux a s1 q with
			| None -> None
			| Some l ->
					let patterns = (p, s2) :: l in
					(* si les motifs sont deux à deux disjoints, on fait un match. Si le
					 * total des motifs ne couvre pas le type du motif, on ajoute un
					 * [M.fail ""]
					 * sinon, on renvoie None *)
					let (exhaustive, disjoint) =
						exhaustive_disjoint sem (Skeleton.pattern_typ p) (List.map fst patterns)
					in
					begin match disjoint (), exhaustive (), s1 with
					| true, true, Return' t ->
						Some (fun ff ->
							Format.fprintf ff "@[<v 0>@[<v 2>begin match%awith@,%a@,end@]"
							(print_bind_value sem) t
							(Format.pp_print_list (fun ff (pat, skel) ->
								Format.fprintf ff "@[<v 4>| %a ->%a@]" (print_pattern sem) pat
							(print_ret_skel sem) skel)) patterns)
					| true, true, _ ->
						Some (fun ff ->
							Format.fprintf ff "@[<v 0>@[<v 2>let* %s =%ain@,\
							begin match %s with@,%a@,end@]"
							!tmp (print_bind_skel sem) s1 !tmp
							(Format.pp_print_list (fun ff (pat, skel) ->
								Format.fprintf ff "@[<v 4>| %a ->%a@]" (print_pattern sem) pat
							(print_ret_skel sem) skel)) patterns)
					| true, false, Return' t ->
						Some (fun ff ->
							Format.fprintf ff "@[<v 2>begin match %a with@,\
							%a@,| _ -> M.fail \"\"@,end"
							(print_bind_value sem) t
							(Format.pp_print_list (fun ff (pat, skel) ->
								Format.fprintf ff "@[<v 4>| %a ->%a@]" (print_pattern sem) pat
							(print_ret_skel sem) skel)) patterns
							)
					| true, false, _ ->
						Some (fun ff ->
							Format.fprintf ff "@[<v 2>let* %s =%ain@,\
							begin match %s with@,%a@,| _ -> M.fail \"\"@,end"
							!tmp (print_bind_skel sem) s1 !tmp
							(Format.pp_print_list (fun ff (pat, skel) ->
								Format.fprintf ff "@[<v 4>| %a ->%a@]" (print_pattern sem) pat
							(print_ret_skel sem) skel)) patterns
							)
					| _ -> None
					end
			end
	| _ -> None
	end




(* Print signature only when necessary (if the value is polymorphic) *)
let print_poly_sig ta (ty: necro_type) ff =
	let ta_print ff ty_print =
		if ta <> [] then
				Format.fprintf ff ": %a. %t" (Util.print_list ~sep:" "
				(fun ff -> Format.fprintf ff "'%s")) ta ty_print
	in
	Format.fprintf ff "%a" ta_print (fun ff -> pp_print_type ff ty)


(* Write the mutually-recursive values *)
let print_value_definitions ff sem =
	let spec_values =
		sem.ss_values
		|> Util.SMap.bindings
		|> List.filter_map (begin function
			| (name, (c, ta, ty, Some t)) ->
					let open Util in
					Some (name, ta, ty, value'_of_value (contents t), Option.map contents c)
			| _ -> None
			end)
	in
	let print_comm ff c =
		begin match c with
		| None -> ()
		| Some c -> Format.fprintf ff "%a@," print_comment c
		end
	in
	let print_value_def ff (name, ta, ty, t, _) =
		Format.fprintf ff "%s%t =@, %a" name
		(print_poly_sig ta ty) (print_value sem) t
	in
	let print_first ff t =
		let (_, _, _, _, c) = t in
		Format.fprintf ff "%a@[<v 2>let rec %a" print_comm c print_value_def t
	in
	let print_not_first ff t =
		let (_, _, _, _, c) = t in
		Format.fprintf ff "@]\n@,%a@[<v 2>and %a" print_comm c print_value_def t
	in
	if spec_values <> [] then
		Format.fprintf ff "\n@,%a%a@]"
		print_first (List.hd spec_values)
		(Util.print_list ~sep:"" print_not_first) (List.tl spec_values)

(* Prints [type t] for all unspecified types *)
let (print_type_decls, print_type_decls_ski) =
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
	(begin fun ff sem ->
		let ts =
			sem.ss_types
			|> Util.SMap.filter (fun _ (_, s,l) -> l = None)
			|> Util.SMap.bindings
			|> List.map (fun (x, (c, ty, _)) -> (Option.map Util.contents c, ty))
		in
		Util.print_list print_type_with_comment ff ts end,
		begin fun ff ski ->
		let ts =
			ski.ski_types
			|> Util.SMap.filter (fun _ (_, s,l) -> l = None)
			|> Util.SMap.bindings
			|> List.map (fun (x, (c, ty, _)) -> (Option.map Util.contents c, ty))
		in
		Util.print_list print_type_with_comment ff ts end
	)


(* Prints [val t: ty] for all unspecified values *)
let print_value_signatures ~all ff sem =
	let print_comment ff c =
			begin match c with
			| None -> ()
			| Some c ->
					Format.fprintf ff "%a@," print_comment c
			end
	in
	(* Prints the signature of a value *)
	let print_value_sig ff c name ty =
		Format.fprintf ff "%aval %s: %a" print_comment c name pp_print_type ty
	in
	let all_sigs =
		sem.ss_values
		|> (if all then Util.id else Util.SMap.filter (fun a (_, _, _, t) -> t = None))
		|> Util.SMap.bindings
	in
	if all_sigs <> [] then
			Format.fprintf ff "\n@,%a"
			(Format.pp_print_list (fun ff (name, (c, _, ty,_)) ->
				print_value_sig ff (Option.map Util.contents c) name ty)) all_sigs

let print_value_signatures_ski ff ski =
	(* Prints the signature of a value *)
	let print_value_sig ff c name ty =
		Format.fprintf ff "val %s: %a" name pp_print_type ty
	in
	let all_sigs =
		ski.ski_values
		|> Util.SMap.bindings
	in
	if all_sigs <> [] then
			Format.fprintf ff "\n@,%a"
			(Format.pp_print_list (fun ff (name, (c, _, ty)) ->
				print_value_sig ff c name ty)) all_sigs

let print_values_not_implemented ff sem =
	let print_value_not_implemented ff (name, _) =
		Format.fprintf ff "let %s _ = raise (NotImplemented \"%s\")" name name
	in
	let vals =
		sem.ss_values
		|> Util.SMap.filter (fun a (_, _, ty, t) ->
				begin match t, Skeleton.unalias ty with
				| None, Arrow _ -> true | _ -> false
				end)
		|> Util.SMap.bindings
	in
	if vals <> [] then
	Format.fprintf ff "\n@,%a"
		(Format.pp_print_list print_value_not_implemented) vals

(** Prints the specified types. If comm is set to true, then also print the
    special comments *)
let (print_specified_types, print_spec_types_ski) =
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
		begin match c.s_input_type with
		| Product [] ->
				Format.fprintf ff "| %s%a"
				c.s_name print_com (Option.map Util.contents c.s_comment)
		| ty ->
				Format.fprintf ff "| %s of %a%a"
				c.s_name print ty print_com (Option.map Util.contents c.s_comment)
		end
	in
	let print_field ff c =
		let print = pp_print_type in
		let print_com ff =
			begin function
			| None -> ()
			| Some c -> Format.fprintf ff " %a" print_comment c
			end
		in
		begin match c.s_output_type with
		| ty ->
				Format.fprintf ff "  %s: %a%a"
				c.s_name print ty print_com (Option.map Util.contents c.s_comment)
		end
	in
	(begin fun ~comm ff ss ->
	let print_comment ff c =
		if not comm then () else
			begin match c with
			| None -> ()
			| Some c ->
					Format.fprintf ff "@,%a" print_comment (Util.contents c)
			end
	in
	let const = Skeleton.get_constructors_by_type ss in
	let fields = Skeleton.get_fields_by_type ss in
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
			(fun ff (ty, c) -> Format.fprintf ff "\n@,%atype %a =@,%a"
				print_comment (get_comm ty) pp_print_type ty
				(Format.pp_print_list print_constructor) c)
			(List.hd const)
				(Util.print_list ~sep:"" (fun ff (ty, c) ->
					Format.fprintf ff "%a@,and %a =@,%a"
					print_comment (get_comm ty) pp_print_type ty
					(Format.pp_print_list print_constructor) c)) (List.tl const);
	if fields <> [] then
		Format.fprintf ff "%a%a"
			(fun ff (ty, f) -> Format.fprintf ff "%s@,%a%s %a = {@,%a@,}"
				(if const = [] then "\n" else "")
				print_comment (get_comm ty)
				(if const = [] then "type" else "and")
				pp_print_type ty
				(Format.pp_print_list ~pp_sep:(fun ff _ ->
					Format.fprintf ff ";@,") print_field) f)
			(List.hd fields)
				(Util.print_list ~sep:"" (fun ff (ty, f) ->
					Format.fprintf ff "@,and %a = {@,%a@,}"
					pp_print_type ty
					(Format.pp_print_list ~pp_sep:(fun ff _ ->
					Format.fprintf ff ";@,") print_field) f)) (List.tl fields) end,
		begin fun ff ski ->
	let get_constructors_and_fields_by_type ski =
		Util.SMap.fold (fun _ (_, s,c) (lc, lf) ->
			begin match s, c with
			| Base (_, _, Variant, _), Some c -> (s,c) :: lc, lf
			| Base (_, _, Record , _), Some c -> lc, (s,c) :: lf
			| _, _ -> (lc, lf)
			end) ski.ski_types ([], [])
	in
	let const, fields = get_constructors_and_fields_by_type ski in
	if const <> [] then
		Format.fprintf ff "%a%a"
			(fun ff (ty, c) -> Format.fprintf ff "\n@,type %a =@,%a"
				pp_print_type ty
				(Format.pp_print_list print_constructor) c)
			(List.hd const)
				(Util.print_list ~sep:"" (fun ff (ty, c) ->
					Format.fprintf ff "@,and %a =@,%a"
					pp_print_type ty
					(Format.pp_print_list print_constructor) c)) (List.tl const) ;
	if fields <> [] then
		Format.fprintf ff "%a%a"
			(fun ff (ty, f) -> Format.fprintf ff "%t %a = {@,%a@,}"
			(fun ff -> Format.fprintf ff (if const = [] then "\n@,type" else "@,and"))
				pp_print_type ty
				(Format.pp_print_list ~pp_sep:(fun ff _ ->
					Format.fprintf ff ";@,") print_field) f)
			(List.hd fields)
				(Util.print_list ~sep:"" (fun ff (ty, f) ->
					Format.fprintf ff "@,and %a = {@,%a@,}"
					pp_print_type ty
					(Format.pp_print_list ~pp_sep:(fun ff _ ->
					Format.fprintf ff ";@,") print_field) f)) (List.tl fields)
		end)

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
				print_comment (Option.map Util.contents c)
				pp_print_type (Base ([], name, Unspec, ta')) pp_print_type ty
			) ff (Util.SMap.bindings ss.ss_aliases)
	| false ->
		Format.pp_print_list ~pp_sep:(fun ff () -> ()) (fun
			ff (name, (c, ta, ty)) ->
				let ta' = List.map (fun a -> Variable a) ta in
				Format.fprintf ff "@,%aand %a = %a"
				print_comment (Option.map Util.contents c)
				pp_print_type (Base ([], name, Unspec, ta')) pp_print_type ty
			) ff (Util.SMap.bindings ss.ss_aliases)
	end

let print_aliases_ski ff ski =
	let empty = Util.SMap.is_empty ski.ski_types in
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
				print_comment (Option.map Util.contents c)
				pp_print_type (Base ([], name, Unspec, ta')) pp_print_type ty
			) ff (Util.SMap.bindings ski.ski_aliases)
	| false ->
		Format.pp_print_list ~pp_sep:(fun ff () -> ()) (fun
			ff (name, (c, ta, ty)) ->
				let ta' = List.map (fun a -> Variable a) ta in
				Format.fprintf ff "@,%aand %a = %a"
				print_comment (Option.map Util.contents c)
				pp_print_type (Base ([], name, Unspec, ta')) pp_print_type ty
			) ff (Util.SMap.bindings ski.ski_aliases)
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
	let includes = Util.SMap.find "this" !dep_map in
	Util.print_list ~sep:"" print_one ff includes

(* writes the UNSPEC module type *)
let print_unspec_module_type sem ff =
	let print_one ff i =
		let l = Util.SMap.find i !dep_map in
		let i' = String.capitalize_ascii i in
		Format.fprintf ff "@,module %s:%s.INTERPRETER with module M = M%a"
		i' i' print_with l
	in
	Format.fprintf ff "(** All types, and the unspecified values *)@,\
		@[<v 2>module type UNSPEC = sig@,%s%a@,%s%a%a%a@]@,end"
		"module M: MONAD"
		(print_includes_unspec print_one) sem
		"include TYPES"
		(print_specified_types ~comm:true) sem
		print_aliases sem
		(print_value_signatures ~all:false) sem

(* writes the TYPES module type *)
let print_types_module_type sem ff =
	Format.fprintf ff "(** The unspecified types *)@,\
		@[<v 2>module type TYPES = sig%a@]@,end"
		print_type_decls sem

let print_monad_module_type sem ff =
	Format.fprintf ff
{|(** The interpretation monad *)
module type MONAD = sig
  type 'a t
  val ret: 'a -> 'a t
  val branch: (unit -> 'a t) list -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val fail: string -> 'a t
  val apply: ('a -> 'b t) -> 'a -> 'b t
end|}

let print_args ff sem =
	let print_include ff (n, sem) =
		Format.fprintf ff "@,(%s:%s.INTERPRETER with module M = F.M)"
		(String.capitalize_ascii n) (String.capitalize_ascii n)
	in
	Util.print_list ~sep:"" print_include ff (Util.SMap.bindings sem.ss_includes)

let print_applys ff apply_max =
	let rec print_x1xn ff n =
		if n = 1 then 
			Format.fprintf ff "x1"
		else
		Format.fprintf ff "%a x%d" print_x1xn (n-1) n
	in
	if apply_max <> 0 then
	Format.fprintf ff "\n@,%s%a"
	"let apply1 f x = M.apply f x"
	(Util.print_list (fun ff i ->
		Format.fprintf ff
		"@,@[<v 2>let apply%d f x0 %a =@,\
		let* _tmp = f x0 in@,\
		apply%d _tmp %a@]"
		i print_x1xn (i-1) (i-1) print_x1xn (i-1)))
	(List.init (apply_max - 1) ((+)2))

(* writes the MakeInterpreter functor *)
let print_makeinterpreter_functor apply_max sem ff =
	Format.fprintf ff
		"(** Module defining the specified values *)@,\
		@[<v 2>module MakeInterpreter (F : UNSPEC)%a = struct@,\
		include F@,let ( let* ) = M.bind%t%a%a@]@,end"
		print_args sem
		(fun ff -> ())
		print_applys apply_max
		print_value_definitions sem



let print_unspec_functor sem ff =
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
		(print_includes_unspec print_one1) sem
		"exception NotImplemented of string"
		"include T"
		"module M = M"
		(print_includes_unspec print_one2) sem
		(print_specified_types ~comm:false) sem
		print_aliases sem
		print_values_not_implemented sem

let print_interpreter_module_type sem ski ff =
	let print_one ff i =
		let i' = String.capitalize_ascii i in
		Format.fprintf ff "@,module %s:%s.INTERPRETER with module M = M"
		i' i'
	in
	Format.fprintf ff
	"(** The module type for interpreters *)@,\
	@[<v 2>module type INTERPRETER = sig@,\
	module M:MONAD%a%a%a%a%a@]@,end" 
	(print_includes print_one) sem
	print_type_decls_ski ski
	(print_spec_types_ski) ski
	print_aliases_ski ski
	(print_value_signatures_ski) ski

let build_map includes ss =
	let rec aux map (i, s) =
		let open Util in
		let incl = SMap.bindings (SMap.map contents s.ski_includes) in
		let map = SMap.add i (List.map fst incl) map in
		List.fold_left aux map incl
	in
	let () = dep_map := aux Util.SMap.empty ("this", Skeleton.ski_of_ss ss) in
	()

(* writes the interpreter generator modules to stdout *)
let generate_interpreter includes sem ski: string =
	let rec apply_max =
		begin function
		| TFunc' (_, _, s) -> apply_max_skel s
		| _ -> 0
		end
	and apply_max_skel =
		begin function
		| Branching' (_, sl) -> List.fold_left max 0 (List.map apply_max_skel sl)
		| LetIn' (Some _, _, s1, s2) ->
				max 2
				(max (apply_max_skel s1) (apply_max_skel s2))
		| LetIn' (None, _, s1, s2) -> max (apply_max_skel s1) (apply_max_skel s2)
		| Return' t -> apply_max t
		| Apply' (_, l1) -> List.length l1
		end
	in
	let apply_max =
		Util.SMap.fold (fun _ (_, _, _, o) i ->
			begin match o with
			| None -> i
			| Some t -> max (apply_max (value'_of_value (Util.contents t))) i
			end) sem.ss_values 0
	in
	let ok x =
		not (List.mem x necro_ocaml_keywords) &&
		not (Skeleton.all_variables_ss sem x) &&
		not (is_keyword x)
	in
	let () = all_includes := includes in
	let () = build_map includes sem in
	let () = tmp := Util.fresh ok !tmp in
	let (sem,ski) =
		Skeleton.protect_ss_and_ski necro_ocaml_keywords (sem,ski)
	in
	let ff = Format.str_formatter in
	let () = Format.fprintf ff
		"%s\n@[<v>%t\n@,%t\n@,%t\n@,%t\n@,%t\n@,%t@]@."
"(** This file was automatically generated using necroml
    See https://gitlab.inria.fr/skeletons/necro-ml/ for more informations *)"
		(print_types_module_type sem) (* module type TYPES *)
		(print_monad_module_type sem) (* module type MONAD *)
		(print_unspec_module_type sem) (* module type UNSPEC *)
		(print_makeinterpreter_functor apply_max sem) (* module MakeInterpreter (F: UNSPEC) …*)
		(print_unspec_functor sem) (* module Unspec (M:MONAD) (T:TYPES) *)
		(print_interpreter_module_type sem ski) (* module type INTERPRETER *)
	in Format.flush_str_formatter ()
