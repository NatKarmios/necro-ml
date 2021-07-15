open Necro
open Skeltypes
open Mlast


(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)
(* =====                Forget locations                 ===== *)
(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)


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

let convert_from from =
	Option.map String.capitalize_ascii from

(* Translate a type into its OCaml embedding *)
let rec get_type (s: necro_type) =
	begin match s with
		| Alias (from, name, ta, _)
		| Base (from, name, _, ta) ->
				MLTyFrom (convert_from from, name, List.map get_type ta)
		| Variable s -> MLTyVar s
		| Product [] -> MLTyUnit
		| Product [_] -> assert false
		| Product l -> MLTyTuple (List.map get_type l)
		| Arrow (s1, s2) -> MLTyArrow (get_type s1,
				MLTyFrom (Some "M", "t", [get_type s2]))
	end

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

let rec get_from_ski from ski =
	begin match from with
	| None -> ski
	| Some a ->
			Util.SMap.find a ski.ski_includes |> fst
	end

(* get an mlpattern out of a skel pattern *)
let rec get_pattern sem p =
	let get_record_field ((from, c, _), _, p) =
		(convert_from from, c, get_pattern sem p)
	in
	begin match p with
	| PWild _ -> MLPWild
	| PVar (v, _) -> MLPVar v
	| PConstr ((from, c, ty), _, PTuple []) ->
			(* if input type for constructor ty is () then the () is ignored. But if
				 it is polymorphic, then OCaml expects the () to be present *)
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
				MLPConstr (convert_from from, c, None)
			else
				MLPConstr (convert_from from, c, Some MLPTop)
	| PConstr ((from, c, _), _, p) ->
			MLPConstr (convert_from from, c, Some (get_pattern sem p))
	| PTuple [] ->
			MLPTop
	| PTuple pl ->
			MLPTuple (List.map (get_pattern sem) pl)
	| PRecord pl ->
			MLPRecord (List.map get_record_field pl)
	end

let rec get_value_of_value sem =
	let get_record_field ((from, c, _), _, t) =
		(convert_from from, c, get_value_of_value sem t)
	in
	begin function
	| TVar (TVLet (n, _)) ->
			MLTVar (None, n)
	| TVar (TVValue (_, f, n, _, _)) ->
			MLTVar (convert_from f, n)
	| TConstr ((from, c, ty), _, TTuple []) ->
			(* if input type for constructor ty is () then the () is ignored. But if
				 it is polymorphic, then OCaml expects the () to be present *)
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
				MLTConstr (convert_from from, c, None)
			else
				MLTConstr (convert_from from, c, Some MLTTop)
	| TConstr ((from, c, _), _, t) ->
			MLTConstr (convert_from from, c, Some (get_value_of_value sem t))
	| TTuple [] -> MLTTop
	| TTuple tl ->
			MLTTuple (List.map (get_value_of_value sem) tl)
	| TFunc (x, _, s) ->
			MLTFunc (MLPVar x, get_value_of_skel sem s)
	| TField (t, ta, (from, f, _)) ->
			begin match int_of_string_opt f with
			| None ->
					MLTField (get_value_of_value sem t, convert_from from, f)
			| Some 1 when List.length ta = 2 ->
					MLTApp (MLTVar (None, "fst"), [get_value_of_value sem t])
			| Some 2 when List.length ta = 2 ->
					MLTApp (MLTVar (None, "snd"), [get_value_of_value sem t])
			| Some i ->
					let l = List.init (List.length ta) (fun j ->
						if j = i - 1 then MLPVar "j" else MLPWild)
					in
					let pattern = MLPTuple l in
					MLTLetIn (pattern, get_value_of_value sem t, MLTVar (None, "x"))
			end
	| TRecord (t, tl) ->
			MLTRecord (
				Option.map (get_value_of_value sem) t,
				List.map get_record_field tl
			)
	end

and get_value_of_skel sem s =
	begin match s with
	| Return t | Apply (t, []) ->
			MLTApp (MLTVar (Some "M", "ret"), [get_value_of_value sem t])
	| Apply (f, q) ->
			let kind =
				begin match f with
				| TVar (TVValue (Unspec, f, n, _, _)) ->
						MLTConstr (Some "M", "Unspec", Some (MLTString n))
				| TVar (TVValue (Spec, f, n, _, _)) ->
						MLTConstr (Some "M", "Spec", Some (MLTString n))
				| _ -> MLTConstr (Some "M", "Anon", None)
				end
			in
			MLTApp (
				MLTVar (None, Format.sprintf "apply%d" (List.length q)),
				kind ::
				get_value_of_value sem f ::
				List.map (get_value_of_value sem) q
			)
	| LetIn (None, p, s1, s2) ->
		if is_exhaustive sem (Skeleton.pattern_typ p) [p] then
			MLTLetStarIn (
				get_pattern sem p,
				get_value_of_skel sem s1,
				get_value_of_skel sem s2
			)
		else
			begin match s1 with
			| Return v1 ->
					MLTMatchWith (
						get_value_of_value sem v1,
						[
							(get_pattern sem p, get_value_of_skel sem s2) ;
							(MLPWild, MLTApp (MLTVar (Some "M", "fail"), [MLTString ""]))
						]
					)
			| _ ->
					MLTLetStarIn (
						MLPVar !tmp,
						get_value_of_skel sem s1,
						MLTMatchWith (
							MLTVar (None, !tmp),
							[
								(get_pattern sem p, get_value_of_skel sem s2) ;
								(MLPWild, MLTApp (MLTVar (Some "M", "fail"), [MLTString ""]))
							]
						)
					)
			end
	| LetIn (Some (k, from, m, ta, _), p, s1, s2)
		when is_exhaustive sem (Skeleton.pattern_typ p) [p] ->
			let kind =
				begin match k with
				| Spec -> MLTConstr (Some "M", "Spec", Some (MLTString m))
				| Unspec -> MLTConstr (Some "M", "Unspec", Some (MLTString m))
				end
			in
			begin match s1 with
			| Return v1 ->
					MLTApp (
						MLTVar (None, "apply2"),
						kind ::
						MLTVar (convert_from from, m) ::
						get_value_of_value sem v1 ::
						MLTFunc (get_pattern sem p, get_value_of_skel sem s2) :: []
					)
			| _ ->
					MLTLetStarIn (
						MLPVar !tmp,
						get_value_of_skel sem s1,
						MLTApp (
							MLTVar (None, "apply2"),
							kind ::
							MLTVar (from, m) ::
							MLTVar (None, !tmp) ::
							MLTFunc (get_pattern sem p, get_value_of_skel sem s2) :: []
						)
					)
			end
	| LetIn (Some (k, from, m, ta, _), p, s1, s2) ->
			let kind =
				begin match k with
				| Spec -> MLTConstr (Some "M", "Spec", Some (MLTString m))
				| Unspec -> MLTConstr (Some "M", "Unspec", Some (MLTString m))
				end
			in
			MLTLetStarIn (
				MLPVar !tmp,
				get_value_of_skel sem s1,
				MLTApp (
					MLTVar (None, "apply2"),
					kind ::
					MLTVar (from, m) ::
					MLTVar (None, !tmp) ::
					MLTFunc (
						MLPVar !tmp,
						get_value_of_skel sem
							(LetIn (None, p, Return (TVar (TVLet (!tmp, Product []))), s2))
					) :: []
				)
			)

	| Branching (_, outs) ->
				begin match branch_to_match sem outs with
				| Some res -> res
				| None ->
						let branches =
							List.map (fun sk ->
								MLTFunc (MLPTop, get_value_of_skel sem sk)
							) outs
						in
						MLTApp (MLTVar (Some "M", "branch"), [MLTList branches])
				end
	| Exists _ -> failwith "Existentials are not supported by Necro ML yet"
	end

and branch_to_match sem outs =
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
	| [] -> Some (MLTVar (Some "M", "fail"))
	| LetIn (a, p, s1, s2) :: q when a = None ->
			begin match aux a s1 q with
			| None -> None
			| Some l ->
					let patterns = (p, s2) :: l in
					(* if patterns are disjoint, we can use a [match with]. If the pattern
					   matching is not exhaustive, we add a catch-all
						 Otherwise, we return [None] to signal we can't do it *)
					let (exhaustive, disjoint) =
						exhaustive_disjoint sem (Skeleton.pattern_typ p) (List.map fst patterns)
					in
					if not (disjoint ()) then None else
					let get_branch (x,y) = 
						(get_pattern sem x, get_value_of_skel sem y)
					in
					let ok_branches = List.map get_branch patterns in
					let fail_branch =
						(MLPWild, MLTApp (MLTVar (Some "M", "fail"),[MLTString ""]))
					in
					let branches =
						if exhaustive () then ok_branches else ok_branches @ [fail_branch]
					in
					Some begin match s1 with
					| Return v1 ->
							MLTMatchWith (get_value_of_value sem v1, branches)
					| _ ->
							MLTLetStarIn (
								MLPVar !tmp,
								get_value_of_skel sem s1,
								MLTMatchWith (MLTVar (None, !tmp), branches)
							)
					end
			end
	| _ -> None
	end




(* Returns signature if it's polymorphic, None otherwise *)
let get_poly_sig ta (ty: necro_type) =
	if ta = [] then None else
	Some (MLTyPoly (ta, get_type ty))


(* Write the mutually-recursive values *)
let get_value_definitions sem =
	let spec_values =
		sem.ss_values
		|> Util.SMap.bindings
		|> List.filter_map (begin function
			| (name, (c, ta, ty, Some t)) ->
					let open Util in
					Some (name, ta, ty, t, c)
			| _ -> None
			end)
	in
	let get_value_decl (name, ta, ty, t, c) =
		(name, c, [], get_poly_sig ta ty, get_value_of_value sem t)
	in
	MRecValDefs (List.map get_value_decl spec_values)


(* returns [type t] for all unspecified types *)
let (unspec_type_decl, unspec_type_decls_ski) =
	(* prints the type's name with "type " before it *)
	let type_with_comment (c, s) =
		begin match s with
		| Product _ | Variable _ | Alias _ | Arrow _ ->
				assert false
		| Base (_, s, _, args) ->
				MTTypeDecl (c, List.map (fun _ -> "_") args, s)
		end
	in
	(begin fun sem ->
		let ts =
			sem.ss_types
			|> Util.SMap.filter (fun _ (_, s,l) -> l = None)
			|> Util.SMap.bindings
			|> List.map (fun (x, (c, ty, _)) -> (c, ty))
		in
		List.map type_with_comment ts end,
	begin fun ski ->
		let ts =
			ski.ski_types
			|> Util.SMap.filter (fun _ (_, s,l) -> l = None)
			|> Util.SMap.bindings
			|> List.map (fun (x, (c, ty, _)) -> (c, ty))
		in
		List.map type_with_comment ts end
	)


(* Prints [val t: ty] for all unspecified values *)
let get_value_signatures ~all sem =
	(* Prints the signature of a value *)
	let get_value_sig (name, (c, _, ty, _)) =
		MTValDecl (c, name, get_type ty)
	in
	let all_sigs =
		sem.ss_values
		|> (if all then Util.id else Util.SMap.filter (fun a (_, _, _, t) -> t = None))
		|> Util.SMap.bindings
	in
	List.map get_value_sig all_sigs

let get_value_signatures_ski ski =
	(* Prints the signature of a value *)
	let get_value_sig (name, (_, c, _, ty)) =
		MTValDecl (c, name, get_type ty)
	in
	let all_sigs =
		ski.ski_values
		|> Util.SMap.bindings
	in
	List.map get_value_sig all_sigs

let get_values_not_implemented sem =
	let get_value_not_implemented (name, _) =
		let error =
			(* raise NotImplemented "%name" *)
			MLTApp (
				MLTVar (None, "raise"),
				[ MLTConstr (None, "NotImplemented", Some (MLTString name)) ]
			)
		in
		MValDef (name, None, ["_"], None, error)
	in
	let vals =
		sem.ss_values
		|> Util.SMap.filter (fun a (_, _, ty, t) ->
				begin match t, Skeleton.unalias ty with
				| None, Arrow _ -> true | _ -> false
				end)
		|> Util.SMap.bindings
	in
	List.map get_value_not_implemented vals

(** Prints the specified types. If comm is set to true, then also print the
r   special comments *)
let (specified_types, spec_types_ski):
	(comm:bool -> 'a -> type_def list) * ('b -> type_def list) =
	let constructor c =
		begin match c.s_input_type with
		| Product [] -> (c.s_name, None, c.s_comment)
		| ty -> (c.s_name, Some (get_type ty), c.s_comment)
		end
	in
	let field c = (c.s_name, get_type c.s_output_type, c.s_comment) in
	let extract =
		List.map (begin function | Variable v -> v | _ -> assert false end)
	in
	(begin fun ~comm ss ->
	let const = Skeleton.get_constructors_by_type ss in
	let fields = Skeleton.get_fields_by_type ss in
	let get_comm base_type =
		if not comm then None else
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
	let variant_type_decls =
		List.map (fun (ty, c) ->
			begin match ty with
			| Base (None, name, Variant, args) ->
					TDVariant (get_comm ty, extract args, name, List.map constructor c)
			| _ -> assert false
			end) const
	in
	let record_type_decls =
		List.map (fun (ty, f) ->
			begin match ty with
			| Base (None, name, Record, args) ->
					TDRecord (get_comm ty, extract args, name, List.map field f)
			| _ -> assert false
			end) fields
	in
	 (variant_type_decls @ record_type_decls) end,
	begin fun ski ->
	let get_constructors_and_fields_by_type ski =
		Util.SMap.fold (fun _ (_, s,c) (lc, lf) ->
			begin match s, c with
			| Base (_, _, Variant, _), Some c -> (s,c) :: lc, lf
			| Base (_, _, Record , _), Some c -> lc, (s,c) :: lf
			| _, _ -> (lc, lf)
			end) ski.ski_types ([], [])
	in
	let const, fields = get_constructors_and_fields_by_type ski in
	let get_comm base_type =
		let name =
			begin match base_type with
			| Base (_, name, _, _) -> name
			| _ -> assert false
			end
		in
		begin match Util.SMap.find_opt name ski.ski_types with
		| Some (c, _, _) -> c
		| None -> failwith "Internal error 1"
		end
	in
	let variant_type_decls =
		List.map (fun (ty, c) ->
			begin match ty with
			| Base (None, name, Variant, args) ->
					TDVariant (get_comm ty, extract args, name, List.map constructor c)
			| _ -> assert false
			end) const
	in
	let record_type_decls =
		List.map (fun (ty, f) ->
			begin match ty with
			| Base (None, name, Record, args) ->
					TDRecord (get_comm ty, extract args, name, List.map field f)
			| _ -> assert false
			end) fields
	in
	(variant_type_decls @ record_type_decls) end)

let get_aliases ss: type_def list =
	let get_alias (name, (c, ta, ty)) =
		TDAlias (c, ta, name, get_type ty)
	in
	List.map get_alias (Util.SMap.bindings ss.ss_aliases)

let get_aliases_ski ski =
	let get_alias (name, (c, ta, ty)) =
		TDAlias (c, ta, name, get_type ty)
	in
	List.map get_alias (Util.SMap.bindings ski.ski_aliases)

(* Function to extract "with module _ = _" *)
let with_modules l =
	let one_with a =
		let a_cap = String.capitalize_ascii a in
		(a_cap, [a_cap])
	in
	("M", ["M"]) :: List.map one_with l

(* Function to print "with module _ = _" *)
let get_with l =
	let get_one_with a =
		let a_cap = String.capitalize_ascii a in
		(a_cap, a_cap)
	in
	List.map get_one_with l

(* writes the UNSPEC module type *)
let unspec_module_type sem =
	let one_include i =
		let l = Util.SMap.find i !dep_map in
		let i' = String.capitalize_ascii i in
		let withes = (with_modules l) in
		MTModule (i', [i'; "INTERPRETER"], withes)
	in
	let com = " All types, and the unspecified values " in
	let def =
		MTBlock (
			MTModule ("M", ["MONAD"], []) ::
			List.map one_include !all_includes @
			[MTInclude "TYPES"])
		:: MTTypeDefs (specified_types ~comm:true sem @ get_aliases sem)
		:: MTBlock (get_value_signatures ~all:false sem)
		:: []
	in
	ModuleType (Some com, "UNSPEC", def)

(* writes the TYPES module type *)
let types_module_type sem =
	let comment = " The unspecified types " in
	ModuleType (Some comment, "TYPES", [MTBlock (unspec_type_decl sem)])

let monad_module_type sem =
	let com = " The interpretation monad " in
	let def =
		let t a = MLTyBase ("t", [MLTyVar a]) in
		[ MTTypeDecl (None, ["a"], "t")
		; MTValDecl ( None,"ret", MLTyArrow (MLTyVar "a", t "a"))
		; MTValDecl ( None,"bind",
			MLTyArrow (t "a", MLTyArrow (MLTyArrow (MLTyVar "a", t "b"), t "b")))
		; MTValDecl (None, "branch",
			MLTyArrow (MLTyList (MLTyArrow (MLTyUnit, t "a")), t "a"))
		; MTValDecl (None, "fail", MLTyArrow (MLTyString, t "a"))
		; MTTypeDefs [ TDVariant (None, [], "kind",
				[ ("Anon", None, None)
				; ("Spec", Some MLTyString, None)
				; ("Unspec", Some MLTyString, None)
				]
			)]
		; MTValDecl (None, "apply",
			let a_bt = MLTyArrow (MLTyVar "a", t "b") in
			MLTyArrow (MLTyBase ("kind", []), MLTyArrow (a_bt, a_bt)))
		; MTValDecl (None, "extract", MLTyArrow (t "a", MLTyVar "a"))
		]
	in
	ModuleType (Some com, "MONAD", [MTBlock def])

let get_module_args sem: modargs =
	let print_include (n, sem) =
		let name = String.capitalize_ascii n in
		(name, [name; "INTERPRETER"], [("M",["F";"M"])])
	in
	List.map print_include (Util.SMap.bindings sem.ss_includes)

let get_opens sem: module_tld list =
	let print_include (n, (_, op)) =
		if op then Some (MOpen [String.capitalize_ascii n]) else None
	in
	List.filter_map print_include (Util.SMap.bindings sem.ss_includes)

let get_applys apply_max =
	let mkvar x = MLTVar (None, x) in
	let rec x2xn n =
		let () = assert (n >= 2) in
		List.init (n-1) (fun i -> Format.sprintf "arg%d" (i+2))
	in
	let apply1 = 
		("apply1", None, [], None, MLTVar (Some "M", "apply"))
	in
	let apply i =
		if i = 1 then apply1 else
		if i < 1 then assert false else
		let x2xi = x2xn i in
		let apply_prev = Format.sprintf "apply%d" (i-1) in
		(
			Format.sprintf "apply%d" i, (* name *)
			None, (* comment *)
			"k" :: "f" :: "arg1" :: x2xi, (* args *)
			None, (* type *)
			MLTLetStarIn (
				MLPVar "_tmp",
				MLTApp (mkvar "apply1", [mkvar "k"; mkvar "f"; mkvar "arg1"]),
				MLTApp (
					mkvar apply_prev,
					MLTConstr (Some "M", "Anon", None)
					:: MLTVar (None, "_tmp")
					:: List.map mkvar x2xi
				)
			) (* body *)
		)
	in
	List.init apply_max (fun i -> MValDef (apply (i+1)))

(* writes the MakeInterpreter functor *)
let makeinterpreter_functor apply_max sem =
	let comment = " Module defining the specified values " in
	let args = ("F",["UNSPEC"],[]) :: get_module_args sem in
	let body =
		[ MInclude ["F"]
		; MValDef ("( let* )", None, [], None, MLTVar (Some "M", "bind"))
		] @ MBlock (get_opens sem)
		:: MBlock (get_applys apply_max)
		:: get_value_definitions sem
		:: []
	in
	Functor (Some comment, "MakeInterpreter", args, body)

let unspec_functor sem =
	let com = " A default instantiation " in
	let args =
		let interp_args =
			List.map (fun name -> 
				let name = String.capitalize_ascii name in
				(name, [name; "INTERPRETER"], ["M", ["M"]])
			) !all_includes
		in
		("M", ["MONAD"], [])
		:: ("T", ["TYPES"], [])
		:: interp_args
	in
	let included_modules =
			List.map (fun name ->
				let name = String.capitalize_ascii name in
				MModuleAlias (name, [name])
			) !all_includes
	in
	let body =
		MBlock (
			MException ("NotImplemented", MLTyString)
			:: MInclude ["T"]
			:: MModuleAlias ("M", ["M"])
			:: included_modules)
		:: MTypeDefs (specified_types ~comm:false sem @ get_aliases sem)
		:: MBlock (get_values_not_implemented sem)
		:: []
	in
	Functor (Some com, "Unspec", args, body)

let interpreter_module_type sem ski =
	let comment = " The module type for interpreters " in
	let includes =
		List.map (fun name ->
			let name = String.capitalize_ascii name in
			MTModule (name, [name;"INTERPRETER"], ["M", ["M"]])
		) (Util.SMap.find "this" !dep_map)
	in
	let body =
		MTModule ("M", ["MONAD"], [])
		:: MTBlock includes
		:: MTBlock (unspec_type_decls_ski ski)
		:: MTTypeDefs (spec_types_ski ski @ get_aliases_ski ski)
		:: MTBlock (get_value_signatures_ski ski)
		:: []
	in
	ModuleType (Some comment, "INTERPRETER", body)

let build_map includes ss =
	let rec aux map (i, (s, _)) =
		let open Util in
		let incl = SMap.bindings s.ski_includes in
		let map = SMap.add i (List.map fst incl) map in
		List.fold_left aux map incl
	in
	let () = dep_map := aux Util.SMap.empty ("this", (Skeleton.ski_of_ss ss, true)) in
	()

(* writes the interpreter generator modules to stdout *)
let generate_interpreter includes sem ski: ast =
	let rec apply_max =
		begin function
		| TFunc (_, _, s) -> apply_max_skel s
		| _ -> 0
		end
	and apply_max_skel =
		begin function
		| Branching (_, sl) -> List.fold_left max 0 (List.map apply_max_skel sl)
		| LetIn (Some _, _, s1, s2) ->
				max 2
				(max (apply_max_skel s1) (apply_max_skel s2))
		| LetIn (None, _, s1, s2) -> max (apply_max_skel s1) (apply_max_skel s2)
		| Return t -> apply_max t
		| Apply (_, l1) -> List.length l1
		| Exists _ -> failwith "Existentials are not supported by Necro ML yet"
		end
	in
	let apply_max =
		Util.SMap.fold (fun _ (_, _, _, o) i ->
			begin match o with
			| None -> i
			| Some t -> max (apply_max t) i
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
	let comm = Comment " This file was automatically generated using necroml
	See https://gitlab.inria.fr/skeletons/necro-ml/ for more informations " in
	let ast =
		[ comm
		; types_module_type sem (* module type TYPES *)
		; monad_module_type sem (* module type MONAD *)
		; unspec_module_type sem (* module type UNSPEC *)
		; unspec_functor sem (* module Unspec (M:MONAD) (T:TYPES) *)
		; interpreter_module_type sem ski (* module type INTERPRETER *)
		; makeinterpreter_functor apply_max sem (* module MakeInterpreter (F: UNSPEC) …*)
		]
	in
	ast
