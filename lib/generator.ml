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
open TypedAST
open Mlast


(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)
(* =====                Forget locations                 ===== *)
(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)


(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)
(* =====          Parameters & useful functions          ===== *)
(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)

let modules () =
  failwith "Necro ML doesn't handle modules yet"

let tmp = ref "_tmp"

let necro_ocaml_keywords = Util.SSet.of_list
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
	begin match s.tydesc with
		| UserType ((n, None), ta) ->
				MLTyBase (n, List.map get_type ta)
    | UserType ((_, Some _), _) -> modules ()
		| Variable s -> MLTyVar s
		| Product [] -> MLTyUnit
		| Product [_] -> assert false
		| Product l -> MLTyTuple (List.map get_type l)
		| Arrow (s1, s2) -> MLTyArrow (get_type s1,
				MLTyFrom (["M"], "t", [get_type s2]))
	end

let get_constructor_signature sem c ty =
  begin match Util.SMap.find ty sem.ss_types with
  | _, TDVariant (_, csl) ->
      List.find (fun cs -> cs.cs_name = c) csl
  | _ -> assert false
  end

(* get an mlpattern out of a skel pattern *)
let rec get_pattern self sem p =
	let get_record_field (((f, x), _), p) =
    if x <> None then modules () else
		(f, get_pattern self sem p)
	in
	begin match p.pdesc with
	| PWild -> MLPWild
	| PVar v -> MLPVar v
  | PConstr (((c, None), (ty, _)), {pdesc=PTuple [];_}) ->
			(* if input type for constructor ty is () then the () is ignored. But if
				 it is polymorphic, then OCaml expects the () to be present *)
      let cdef = get_constructor_signature sem c ty in
			let input = cdef.cs_input_type in
			if input.tydesc = Product [] then
				MLPConstr (c, None)
			else
				MLPConstr (c, Some MLPTop)
	| PConstr (((c, None), _), p) ->
			MLPConstr (c, Some (get_pattern self sem p))
  | PConstr (((_, Some _), _), _) -> modules ()
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


let is_exhaustive sem p =
  let wild = {pdesc=PWild;ploc=Util.ghost_loc;ptyp=p.ptyp} in
  not @@ Skeleton.useful [] sem [[p]] [wild]

let rec get_term_of_term self sem t =
	let get_record_field (((f, m), _), t) =
    if m <> None then modules () else
		(f, get_term_of_term self sem t)
	in
	begin match t.tdesc with
	| TVar (LetBound (n, _)) ->
			MLTVar ([], n)
	| TVar (TopLevel (_, (x, m), _, _)) ->
      if m <> None then modules () else
			MLTVar ([], x)
	| TConstr (((c, m), (ty, _)), {tdesc=TTuple [];_}) ->
			(* if input type for constructor ty is () then the () is ignored. But if
				 it is polymorphic, then OCaml expects the () to be present *)
      if Option.is_some m then modules () else
			let cdef = get_constructor_signature sem c ty in
			let input = cdef.cs_input_type in
			if input.tydesc = Product [] then
				MLTConstr (c, None)
			else
				MLTConstr (c, Some MLTTop)
	| TConstr (((c, m), _), t) ->
      if Option.is_some m then modules () else
			MLTConstr (c, Some (get_term_of_term self sem t))
	| TTuple [] -> MLTTop
	| TTuple tl ->
			MLTTuple (List.map (get_term_of_term self sem) tl)
	| TFunc (p, s) ->
			if is_exhaustive sem p then
				MLTFunc (get_pattern self sem p, get_term_of_skel self sem s)
			else
				MLTFunction (
					(get_pattern self sem p, get_term_of_skel self sem s) ::
				(MLPWild, MLTApp (MLTVar (["M"], "fail"), [MLTTop])) :: [])
	| TField (t, ((f, m), _)) ->
      if Option.is_some m then modules () ;
			MLTField (get_term_of_term self sem t, f)
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
				if j = i - 1 then MLPVar "x" else MLPWild)
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
			if is_exhaustive sem p
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
		( TermBind (_, (b, m), _, _) |
			SymbolBind (_, _, (b, m), _, _)), p, s1, s2) ->
        if Option.is_some m then modules () else
			if is_exhaustive sem p then
				begin match s1.sdesc with
				| Return t1 ->
						(* bind (t1, λ p → s2) *)
						MLTApp ( MLTVar ([], b),
							[MLTTuple ( get_term_of_term self sem t1 ::
								MLTFunc (get_pattern self sem p, get_term_of_skel self sem s2) :: [])])
				| _ ->
						(* let* tmp = s1 in
						   bind (tmp, λ p → s2) *)
						MLTLetStarIn (
							MLPVar !tmp, get_term_of_skel self sem s1,
							MLTApp ( MLTVar ([], b),
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
				MLTApp ( MLTVar ([], b),
					[MLTTuple (MLTVar ([], !tmp) ::
						get_term_of_term self sem
						{tdesc=TFunc (p, s2);tloc=Util.ghost_loc;ttyp=
              {tydesc=Product [];tyloc=Util.ghost_loc}} :: [])]
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
	let def (t, _path) =
		begin match Util.SMap.find_opt t spec_terms with
		| None -> assert false
		| Some x -> x
		end
	in
	let dep = Skeleton.term_dep_graph [] sem in
	let scc_all =
		let module SCC = Graph.Components.Make(Skeleton.PGraph) in
		SCC.scc_list dep
	in
	(* scc_all is a list of all strongly connected component of graph [dep],
		 listed in topological order *)
	let scc =
		(* We filter out unspecified terms *)
		List.map (List.filter (fun (name, path) ->
			assert (path = None);
			begin match Util.SMap.find_opt name sem.ss_terms with
			| None -> assert false
			| Some (_, (_, _, o)) -> Option.is_some o
			end)) scc_all
		|> List.filter ((<>) []) |> List.rev
	in
	List.map (fun terms ->
		begin match terms with
		| [] -> assert false (* contains at least v *)
		| _ :: _ :: _ -> (* at least two recursive terms *)
				MRecValDefs (List.map def terms)
		| t :: [] ->
				let t_out = Skeleton.PGraph.succ dep t in
				let recursive = List.mem t t_out in
				if recursive then MRecValDefs [def t]
				else MValDef (def t)
		end) scc


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

let get_terms_not_implemented sem =
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
				begin match t, Skeleton.unalias [] sem ty with
        | None, {tydesc=Arrow _;_} -> true | _ -> false
				end)
		|> Util.SMap.bindings
	in
	List.map get_term_not_implemented terms

(** Prints the specified types. If comm is set to true, then also print the
r   special comments *)
let specified_types ~comm ss: type_def list =
	let constructor c =
		begin match c.cs_input_type with
    | {tydesc=Product [];_} -> (c.cs_name, None, c.cs_comment)
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
			MLTDVariant (get_comm ty, ta, ty, None, List.map constructor c)) const
	in
	let record_type_decls =
		List.map (fun (ty, ta, f) ->
			MLTDRecord (get_comm ty, ta, ty, None, List.map field f)) fields
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

(* writes the MakeInterpreter functor *)
let makeinterpreter_functor self sem =
	let comment = " Module defining the specified terms " in
	let args = ("F",["UNSPEC"],[])  in
	let body =
		MModuleAlias ("M", ["F"; "M"]) ::
		MOpenStruct (
			MInclude ["F"] ::
			MValDef ("( let* )", None, [], None, MLTVar (["M"], "bind")) ::
    get_term_definitions self sem) :: []
	in
	Functor (Some comment, "MakeInterpreter", [args], body)

let unspec_functor sem =
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
		:: MBlock (get_terms_not_implemented sem)
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
let generate_interpreter includes (self, sem): ast =
  if not @@ List.is_empty includes then modules () ;
	let sem, _protect = Skeleton.protect_ss necro_ocaml_keywords sem in
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
		; unspec_functor sem (* module Unspec (M:MONAD) (T:TYPES) *)
		; interpreter_module_type sem (* module type INTERPRETER *)
		; makeinterpreter_functor self sem (* module MakeInterpreter (F: UNSPEC) …*)
		]
	in
	ast
