(**************************************************************************)
(*                                                                        *)
(*                             Necro Library                              *)
(*                                                                        *)
(*                 Victoire Noizet, projet Épicure, Inria                 *)
(*                                                                        *)
(*   Copyright 2022 INRIA.                                                *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU General Public License version 3 as described in the file    *)
(*   LICENSE.                                                             *)
(*                                                                        *)
(**************************************************************************)

open Necro
open ParsedAST
open TypedAST
open Mlast
open Necrotrans


(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)
(* =====                Forget locations                 ===== *)
(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)


(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)
(* =====          Parameters & useful functions          ===== *)
(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)

let tmp = ref "_tmp"

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
	]

(* Translate a type into its OCaml embedding *)
let rec get_type (s: typ) =
	begin match s with
		| UserType (n, ta) ->
				MLTyBase (n.obj, List.map get_type ta)
		| Variable s -> MLTyVar s
		| Product [] -> MLTyUnit
		| Product [_] -> assert false
		| Product l -> MLTyTuple (List.map get_type l)
		| Arrow (s1, s2) -> MLTyArrow (get_type s1,
				MLTyFrom (["M"], "t", [get_type s2]))
	end

(* get an mlpattern out of a skel pattern *)
let rec get_pattern self sem p =
	let get_record_field ((f, _), p) =
		(f.obj, get_pattern self sem p)
	in
	begin match p with
	| PWild _ -> MLPWild
	| PVar (v, _) -> MLPVar v
	| PConstr ((c, _), PTuple []) ->
			(* if input type for constructor ty is () then the () is ignored. But if
				 it is polymorphic, then OCaml expects the () to be present *)
			let cdef = Skeleton.get_constructor_signature self [] sem c in
			let input = cdef.cs_input_type in
			if input = Product [] then
				MLPConstr (c.obj, None)
			else
				MLPConstr (c.obj, Some MLPTop)
	| PConstr ((c, _), p) ->
			MLPConstr (c.obj, Some (get_pattern self sem p))
	| PTuple [] ->
			MLPTop
	| PTuple pl ->
			MLPTuple (List.map (get_pattern self sem) pl)
	| PRecord pl ->
			MLPRecord (List.map get_record_field pl)
	| POr (p1, p2) ->
			MLPOr (get_pattern self sem p1, get_pattern self sem p2)
	| PType (p, _) -> get_pattern self sem p
	end

let rec get_term_of_term self sem t =
	let get_record_field ((f, _), t) =
		(f.obj, get_term_of_term self sem t)
	in
	begin match t.tdesc with
	| TVar (LetBound (n, _)) ->
			MLTVar ([], n)
	| TVar (TopLevel (_, x, _, _)) ->
			MLTVar ([], x.obj)
	| TConstr ((c, _), {tdesc=TTuple [];_}) ->
			(* if input type for constructor ty is () then the () is ignored. But if
				 it is polymorphic, then OCaml expects the () to be present *)
			let cdef = Skeleton.get_constructor_signature self [] sem c in
			let input = cdef.cs_input_type in
			if input = Product [] then
				MLTConstr (c.obj, None)
			else
				MLTConstr (c.obj, Some MLTTop)
	| TConstr ((c, _), t) ->
			MLTConstr (c.obj, Some (get_term_of_term self sem t))
	| TTuple [] -> MLTTop
	| TTuple tl ->
			MLTTuple (List.map (get_term_of_term self sem) tl)
	| TFunc (p, s) ->
			if Skeleton.is_exhaustive self [] sem [p] then
				MLTFunc (get_pattern self sem p, get_term_of_skel self sem s)
			else
				MLTFunction (
					(get_pattern self sem p, get_term_of_skel self sem s) ::
				(MLPWild, MLTApp (MLTVar (["M"], "fail"), [MLTTop])) :: [])
	| TField (t, (f, _)) ->
			MLTField (get_term_of_term self sem t, f.obj)
	| TNth (t, tyl, i) ->
			if List.compare_length_with tyl 2 = 0 then
			begin
				if i = 1 then
					MLTApp (MLTVar ([], "fst"), [get_term_of_term self sem t])
				else
					MLTApp (MLTVar ([], "snd"), [get_term_of_term self sem t])
			end
			else
			let l = List.init (List.length tyl) (fun j ->
				if j = i - 1 then MLPVar "j" else MLPWild)
			in
			let pattern = MLPTuple l in
			MLTLetIn (pattern, get_term_of_term self sem t, MLTVar ([], "x"))
	| TRecMake tl ->
			MLTRecMake (List.map get_record_field tl)
	| TRecSet (t, tl) ->
			MLTRecWith (
				get_term_of_term self sem t,
				List.map get_record_field tl
			)
	end

and get_term_of_skel self sem s =
	begin match s.sdesc with
	| Return t ->
			MLTApp (MLTVar (["M"], "ret"), [get_term_of_term self sem t])
	| Apply (f, x) ->
			MLTApp (
				MLTVar (["M"], "apply"),
				get_term_of_term self sem f :: get_term_of_term self sem x :: []
			)
	| LetIn (NoBind, p, s1, s2) ->
			if Skeleton.is_exhaustive self [] sem [p]
		then
			MLTLetStarIn (
				get_pattern self sem p,
				get_term_of_skel self sem s1,
				get_term_of_skel self sem s2
			)
		else
			begin match s1.sdesc with
			| Return t1 ->
					MLTMatchWith (
						get_term_of_term self sem t1,
						[
							(get_pattern self sem p, get_term_of_skel self sem s2) ;
							(MLPWild, MLTApp (MLTVar (["M"], "fail"), [MLTTop]))
						]
					)
			| _ ->
					MLTLetStarIn (
						MLPVar !tmp,
						get_term_of_skel self sem s1,
						MLTMatchWith (
							MLTVar ([], !tmp),
							[
								(get_pattern self sem p, get_term_of_skel self sem s2) ;
								(MLPWild, MLTApp (MLTVar (["M"], "fail"), [MLTTop]))
							]
						)
					)
			end
	| LetIn (
		( TermBind (_, b, _, _) |
			SymbolBind (_, _, b, _, _)), p, s1, s2) ->
			if Skeleton.is_exhaustive self [] sem [p] then
				begin match s1.sdesc with
				| Return t1 ->
						(* bind (t1, λ p → s2) *)
						MLTApp ( MLTVar ([], b.obj),
							[MLTTuple ( get_term_of_term self sem t1 ::
								MLTFunc (get_pattern self sem p, get_term_of_skel self sem s2) :: [])])
				| _ ->
						(* let* tmp = s1 in
						   bind (tmp, λ p → s2) *)
						MLTLetStarIn (
							MLPVar !tmp, get_term_of_skel self sem s1,
							MLTApp ( MLTVar ([], b.obj),
								[MLTTuple (MLTVar ([], !tmp) ::
									MLTFunc (get_pattern self sem p, get_term_of_skel self sem s2) :: [])]
							)
						)
				end
			else
						(* let* tmp = s1 in
						   bind (tmp, s')
							 where s' = is the exhaustive version of [λ p → s2] *)
			MLTLetStarIn (
				MLPVar !tmp, get_term_of_skel self sem s1,
				MLTApp ( MLTVar ([], b.obj),
					[MLTTuple (MLTVar ([], !tmp) ::
						get_term_of_term self sem
						{tdesc=TFunc (p, s2);tloc=Util.ghost_loc;ttyp=Product []} :: [])]
				)
			)
	| Branching (_, outs) ->
			let branches =
				List.map (fun sk ->
					MLTFunc (MLPTop, get_term_of_skel self sem sk)
				) outs
			in
			MLTApp (MLTVar (["M"], "branch"), [MLTList branches])
	| Match ({sdesc=Return tm;_}, outs) ->
			let branches =
				List.map (Pair.map (get_pattern self sem) (get_term_of_skel self sem)) outs
			in
			MLTMatchWith (get_term_of_term self sem tm, branches)
	| Match (sm, outs) ->
			let branches =
				List.map (Pair.map (get_pattern self sem) (get_term_of_skel self sem)) outs
			in
			(* let* _tmp = sm in … *)
			MLTLetStarIn (
				MLPVar !tmp, get_term_of_skel self sem sm,
				MLTMatchWith (get_term_of_skel self sem sm, branches))
	| Exists _ -> failwith "Existentials are not supported by Necro ML yet"
	end



(* Returns signature if it's polymorphic, None otherwise *)
let get_poly_sig ta (ty: typ) =
	if ta = [] then None else
	Some (MLTyPoly (ta, get_type ty))


(* Write the mutually-recursive terms *)
let get_term_definitions self sem =
	let open Util.SMap in
	let get_term_decl name (ta, ty, t, c) =
		(name, c, [], get_poly_sig ta ty, get_term_of_term self sem t)
	in
	let spec_terms =
		sem.ss_terms
		|> filter (fun _ (_, (_, _, o)) -> Option.is_some o)
		|> map (begin function
			| (c, (ta, ty, Some t)) ->
					(ta, ty, t, c)
			| _ -> assert false
			end)
		|> mapi get_term_decl
	in
	let def (_path, t) =
		begin match Util.SMap.find_opt t spec_terms with
		| None -> assert false
		| Some x -> x
		end
	in
	let dep = Skeleton.dependency_graph self sem in
	let scc = Skeleton.PGraph.scc dep in
	let scc_map =
		List.fold_left (fun m l ->
			begin match l with
			| [] -> assert false (* scc failed *)
			| (_path, a) :: _ -> (* no module at this point anyway *)
					Util.SMap.add a l m
			end) Util.SMap.empty scc
	in
	let dep_contracted = List.fold_left (fun dep component ->
		Skeleton.PGraph.contract dep component) dep scc
	in
	let topo = Skeleton.PGraph.topo_sort dep_contracted in
	List.map (fun (_, v) ->
		let terms = Util.SMap.find v scc_map in
		begin match terms with
		| [] -> assert false (* contains at least v *)
		| _ :: _ :: _ -> (* at least two recursive terms *)
				MRecValDefs (List.map def terms)
		| t :: [] ->
				let t_out = Skeleton.PGraph.next dep t in
				let recursive = List.mem t t_out in
				if recursive then MRecValDefs [def t]
				else MValDef (def t)
		end) topo


(* returns [type t] for all unspecified types *)
let unspec_type_decl sem =
	(* prints the type's name with "type " before it *)
	let type_with_comment (com, typename, args) =
		MTTypeDecl (com, List.init args (fun _ -> None), typename)
	in
	sem.ss_types
	|> Util.SMap.filter_map (fun _ (com, td) ->
			begin match td with TDUnspec i -> Some (com, i) | _ -> None end)
	|> Util.SMap.bindings
	|> List.map (fun (x, (c, i)) -> type_with_comment (c, x, i))


(* Prints [val t: ty] for all unspecified terms *)
let get_term_signatures ~all sem =
	(* Prints the signature of a term *)
	let get_term_sig (name, (c, (_, ty, _))) =
		MTValDecl (c, name, get_type ty)
	in
	let all_sigs =
		sem.ss_terms
		|> (if all then Fun.id else Util.SMap.filter (fun _ (_, (_, _, t)) -> t = None))
		|> Util.SMap.bindings
	in
	List.map get_term_sig all_sigs

let get_terms_not_implemented self sem =
	let get_term_not_implemented (name, _) =
		let error =
			(* raise NotImplemented "%name" *)
			MLTApp (
				MLTVar ([], "raise"),
				[ MLTConstr ("NotImplemented", Some (MLTString name)) ]
			)
		in
		MValDef (name, None, ["_"], None, error)
	in
	let terms =
		sem.ss_terms
		|> Util.SMap.filter (fun _ (_, (_, ty, t)) ->
				begin match t, Skeleton.unalias self [] sem ty with
				| None, Arrow _ -> true | _ -> false
				end)
		|> Util.SMap.bindings
	in
	List.map get_term_not_implemented terms

(** Prints the specified types. If comm is set to true, then also print the
r   special comments *)
let specified_types ~comm ss: type_def list =
	let constructor c =
		begin match c.cs_input_type with
		| Product [] -> (c.cs_name, None, c.cs_comment)
		| ty -> (c.cs_name, Some (get_type ty), c.cs_comment)
		end
	in
	let field f = (f.fs_name, get_type f.fs_field_type, f.fs_comment) in
	let const = Skeleton.get_constructors_by_type ss in
	let fields = Skeleton.get_fields_by_type ss in
	let get_comm name =
		if not comm then None else
		begin match Util.SMap.find_opt name ss.ss_types with
		| Some (c, _) -> c
		| None -> failwith "Internal error 1"
		end
	in
	let variant_type_decls =
		List.map (fun (ty, ta, c) ->
			MLTDVariant (get_comm ty, ta, ty, List.map constructor c)) const
	in
	let record_type_decls =
		List.map (fun (ty, ta, f) ->
			MLTDRecord (get_comm ty, ta, ty, List.map field f)) fields
	in
	 (variant_type_decls @ record_type_decls)

let get_aliases ss: type_def list =
	let aliases = Util.SMap.filter_map (fun _ (com, td) ->
		begin match td with
		| TDAlias (ta, ty) -> Some (com, ta, ty)
		| _ -> None
		end) ss.ss_types
	in
	let get_alias (name, (c, ta, ty)) =
		MLTDAlias (c, ta, name, get_type ty)
	in
	List.map get_alias (Util.SMap.bindings aliases)

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
	let com = " All types, and the unspecified terms " in
	let def =
		MTBlock (
			MTModule ("M", ["MONAD"], []) ::
			[MTInclude "TYPES"])
		:: MTTypeDefs (specified_types ~comm:true sem @ get_aliases sem)
		:: MTBlock (get_term_signatures ~all:false sem)
		:: []
	in
	ModuleType (Some com, "UNSPEC", def)

(* writes the TYPES module type *)
let types_module_type sem =
	let comment = " The unspecified types " in
	ModuleType (Some comment, "TYPES", [MTBlock (unspec_type_decl sem)])

let monad_module_type _sem =
	let com = " The interpretation monad " in
	let def =
		let t a = MLTyBase ("t", [MLTyVar a]) in
		[ MTTypeDecl (None, [Some "a"], "t")
		; MTValDecl ( None,"ret", MLTyArrow (MLTyVar "a", t "a"))
		; MTValDecl ( None,"bind",
			MLTyArrow (t "a", MLTyArrow (MLTyArrow (MLTyVar "a", t "b"), t "b")))
		; MTValDecl (None, "branch",
			MLTyArrow (MLTyList (MLTyArrow (MLTyUnit, t "a")), t "a"))
		; MTValDecl (None, "fail", MLTyArrow (MLTyUnit, t "a"))
		; MTValDecl (None, "apply",
			let a_bt = MLTyArrow (MLTyVar "a", t "b") in
			MLTyArrow (a_bt, a_bt))
		; MTValDecl (None, "extract", MLTyArrow (t "a", MLTyVar "a"))
		]
	in
	ModuleType (Some com, "MONAD", [MTBlock def])

(*
let get_module_args sem: modargs =
	let print_include (n, _sem) =
		let name = String.capitalize_ascii n in
		(name, [name; "INTERPRETER"], [("M",["F";"M"])])
	in
	List.map print_include (Util.SMap.bindings sem.ss_includes)
*)

(*
let get_opens sem: module_tld list =
	let print_include (n, (_, op)) =
		if op then Some (MOpen [String.capitalize_ascii n]) else None
	in
	List.filter_map print_include (Util.SMap.bindings sem.ss_includes)
*)

(* recreate the module structure (unflatten) *)
let remake_module (remappings:Transformation.remapping) sem_init self =
	let open Transformation in
	let rec aux sem path =
		let make_decl d =
			begin match d with
			| ParsedAST.TypeDecl x ->
					let fullname =
						begin match PMap.find_opt (path, x) remappings.types with
						| Some x' -> x'
						| None -> x
						end
					in
					Some (MTypeNonRec (MLTDAlias (None, [(* FIXME *)], x,
						MLTyBase (fullname, [(* FIXME *)]))))
			| ParsedAST.TermDecl x ->
					let fullname =
						begin match PMap.find_opt (path, x) remappings.terms with
						| Some x' -> x'
						| None -> x
						end
					in
					Some (MValDef (x, None, [], None, MLTVar ([], fullname)))
			| ParsedAST.BinderDecl _ ->
					None
			| ParsedAST.ModuleDecl m ->
					let (_, ss_m) = Util.SMap.find m sem.ss_modules in
					Some (MModule (String.capitalize_ascii m, aux ss_m (Path.Abs.append path m)))
			| ParsedAST.Comment c ->
					Some (MComment c.contents)
			end
		in
		List.filter_map make_decl sem.ss_order
	in aux sem_init (Path.Abs.File (self, None))

(* writes the MakeInterpreter functor *)
let makeinterpreter_functor self sem remappings sem_init =
	let comment = " Module defining the specified terms " in
	let args = ("F",["UNSPEC"],[])  in
	let body =
		[ MInclude ["F"]
		; MValDef ("( let* )", None, [], None, MLTVar (["M"], "bind"))
		] @
		MOpenStruct (get_term_definitions self sem)
		:: remake_module remappings sem_init self
	in
	Functor (Some comment, "MakeInterpreter", [args], body)

let unspec_functor self sem =
	let com = " A default instantiation " in
	let args =
		("M", ["MONAD"], [])
		:: ("T", ["TYPES"], []) :: []
	in
	let body =
		MBlock (
			MException ("NotImplemented", MLTyString)
			:: MInclude ["T"]
			:: MModuleAlias ("M", ["M"]) :: [])
		:: MTypeDefs (specified_types ~comm:false sem @ get_aliases sem)
		:: MBlock (get_terms_not_implemented self sem)
		:: []
	in
	Functor (Some com, "Unspec", args, body)

let interpreter_module_type sem =
	let comment = " The module type for interpreters " in
	let body =
		MTModule ("M", ["MONAD"], [])
		(*:: MTBlock includes*)
		:: MTBlock (unspec_type_decl sem)
		:: MTTypeDefs (specified_types ~comm:false sem @ get_aliases sem)
		:: MTBlock (get_term_signatures ~all:true sem)
		:: []
	in
	ModuleType (Some comment, "INTERPRETER", body)

(* writes the interpreter generator modules to stdout *)
let generate_interpreter includes (self, sem_init): ast =
	let sem, remappings =
		Transformation.flatten_modules ["-f";"-c"] includes (self, sem_init) in
	let version =
		begin match Build_info.V1.version () with
		| None -> "N/A"
		| Some v -> Build_info.V1.Version.to_string v
		end
	in
	let comm = Comment
	(" This file was automatically generated using necroml version " ^ version ^
	".\nSee https://gitlab.inria.fr/skeletons/necro-ml/ for more informations. ") in
	let ast =
		[ comm
		; types_module_type sem (* module type TYPES *)
		; monad_module_type sem (* module type MONAD *)
		; unspec_module_type sem (* module type UNSPEC *)
		; unspec_functor self sem (* module Unspec (M:MONAD) (T:TYPES) *)
		; interpreter_module_type sem (* module type INTERPRETER *)
		; makeinterpreter_functor self sem remappings sem_init (* module MakeInterpreter (F: UNSPEC) …*)
		]
	in
	ast
