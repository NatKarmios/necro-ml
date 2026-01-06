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

let tmp = ref "_tmp"

let necro_ocaml_keywords = SSet.of_list
  [
  (* OCaml reserved keywords *)
  "and"; "as"; "asr"; "assert"; "begin"; "class"; "constraint"; "do"; "done";
  "downto"; "effect"; "else"; "end"; "exception"; "external"; "false"; "for";
  "fun"; "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer";
  "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match"; "method"; "mod";
  "module"; "open"; "mutable"; "new"; "nonrec"; "object"; "of"; "open"; "or";
  "private"; "rec"; "sig"; "struct"; "then"; "to"; "true"; "try"; "type"; "val";
  "virtual"; "when"; "while"; "with" ;
  ]

let cap = String.capitalize_ascii
let of_from x = Option.to_list x |> List.map cap

(* Translate a type into its OCaml embedding *)
let rec get_type (s: typ) =
  begin match s.tydesc with
    | UserType ((n, None), ta) ->
        MLTyBase (n, List.map get_type ta)
    | UserType ((n, Some x), ta) ->
        MLTyFrom ([cap x], n, List.map get_type ta)
    | Variable s -> MLTyVar s
    | Product [] -> MLTyUnit
    | Product [_] -> assert false
    | Product l -> MLTyTuple (List.map get_type l)
    | Arrow (s1, s2) -> MLTyArrow (get_type s1,
        MLTyFrom (["M"], "t", [get_type s2]))
  end

let get_constructor_signature deps sem (c, m) _ty =
  Skeleton.get_constructor_signature deps sem
  (c, m)

(* get an mlpattern out of a skel pattern *)
let rec get_pattern deps sem p =
  let get_record_field (((f, _), _), p) =
    (f, get_pattern deps sem p)
  in
  begin match p.pdesc with
  | PWild -> MLPWild
  | PVar v -> MLPVar v
  | PConstr (((c, m), (ty, _)), {pdesc=PTuple [];_}) ->
      (* if input type for constructor ty is () then the () is ignored. But if
         it is polymorphic, then OCaml expects the () to be present *)
      let cdef = get_constructor_signature deps sem (c, m) ty in
      let cdef = Option.get cdef in
      let input = cdef.cs_input_type in
      if input.tydesc = Product [] then
        MLPConstr ((c, Option.map String.capitalize_ascii m), None)
      else
        MLPConstr ((c, Option.map String.capitalize_ascii m), Some MLPTop)
  | PConstr (((c, m), _), p) ->
      MLPConstr ((c, Option.map String.capitalize_ascii m), Some (get_pattern
      deps sem p))
  | PTuple [] ->
      MLPTop
  | PTuple pl ->
      MLPTuple (List.map (get_pattern deps sem) pl)
  | PRecord [] -> assert false (* forbidden by lexing *)
  | PRecord (((((_, path), (typname, _)), _) :: _ ) as pl) ->
     let (_, typdef) = SMap.find typname sem.ss_types in
     begin match typdef with
     | TDUnspec _ | TDVariant _ | TDAlias _ -> assert false
     | TDRecord (_, fss) ->
        let all_field_matched = List.length fss = List.length pl in
        MLPRecord (of_from path, List.map get_record_field pl, all_field_matched)
     end
  | POr (p1, p2) ->
      MLPOr (get_pattern deps sem p1, get_pattern deps sem p2)
  | PType (p, _) -> get_pattern deps sem p
  end


let is_exhaustive _sem p =
  let rec is_tuple_var p =
    begin match p.pdesc with
    | PVar _ | PWild -> true
    | PTuple pl -> List.for_all is_tuple_var pl
    | PType (p, _) -> is_tuple_var p
    | PRecord fpl -> List.for_all (fun (_, p) -> is_tuple_var p) fpl
    | PConstr _ | POr _ -> false
    end
  in
  is_tuple_var p
  (* TODO : fix
  let wild = {pdesc=PWild;ploc=Util.ghost_loc;ptyp=p.ptyp} in
  try
  not @@ Skeleton.useful [] sem [[p]] [wild]
  with Not_found ->
    print_endline (Necro.Printer.string_of_pattern p); assert false
    *)

let rec get_term_of_term deps sem t =
  let get_record_field (((f, _), _), t) =
    (f, get_term_of_term deps sem t)
  in
  begin match t.tdesc with
  | TVar (LetBound (n, _)) ->
      MLTVar ([], n)
  | TVar (TopLevel (_, (x, None), _)) ->
      MLTVar ([], x)
  | TVar (TopLevel (_, (x, Some m), _)) ->
      MLTVar ([cap m], x)
  | TConstr (((c, m), (ty, _)), {tdesc=TTuple [];_}) ->
      (* if input type for constructor ty is () then the () is ignored. But if
         it is polymorphic, then OCaml expects the () to be present *)
      let cdef = get_constructor_signature deps sem (c, m) ty in
      let cdef = Option.get cdef in
      let input = cdef.cs_input_type in
      if input.tydesc = Product [] then
        MLTConstr (of_from m, c, None)
      else
        MLTConstr (of_from m, c, Some MLTTop)
  | TConstr (((c, m), _), t) ->
      MLTConstr (of_from m, c, Some (get_term_of_term deps sem t))
  | TTuple [] -> MLTTop
  | TTuple tl ->
      MLTTuple (List.map (get_term_of_term deps sem) tl)
  | TFunc (p, s) ->
      if is_exhaustive sem p then
        MLTFunc (get_pattern deps sem p, get_term_of_skel deps sem s)
      else
        MLTFunction (
          (get_pattern deps sem p, get_term_of_skel deps sem s) ::
        (MLPWild, MLTApp (MLTVar (["M"], "fail"), [MLTTop])) :: [])
  | TField (t, ((f, _), _)) ->
      MLTField (get_term_of_term deps sem t, f)
  | TNth (t, tyl, i) ->
      if List.compare_length_with tyl 2 = 0 then
      begin
        if i = 1 then
          MLTApp (MLTVar ([], "fst"), [get_term_of_term deps sem t])
        else
          MLTApp (MLTVar ([], "snd"), [get_term_of_term deps sem t])
      end
      else
      let l = List.init (List.length tyl) (fun j ->
        if j = i - 1 then MLPVar "x" else MLPWild)
      in
      let pattern = MLPTuple l in
      MLTLetIn (pattern, get_term_of_term deps sem t, MLTVar ([], "x"))
  | TRecMake [] -> assert false
  | TRecMake (((((_, f), _), _) :: _) as tl) ->
      MLTRecMake (of_from f, List.map get_record_field tl)
  | TRecSet (t, tl) ->
      MLTRecWith (
        get_term_of_term deps sem t,
        List.map get_record_field tl
      )
  end

and get_term_of_skel deps sem s =
  begin match s.sdesc with
  | Return t ->
      MLTApp (MLTVar (["M"], "ret"), [get_term_of_term deps sem t])
  | Apply (f, x) ->
      MLTApp (
        MLTVar (["M"], "apply"),
        get_term_of_term deps sem f :: get_term_of_term deps sem x :: []
      )
  | LetIn (NoBind, p, s1, s2) ->
      if is_exhaustive sem p
    then
      MLTLetStarIn (
        get_pattern deps sem p,
        get_term_of_skel deps sem s1,
        get_term_of_skel deps sem s2
      )
    else
      begin match s1.sdesc with
      | Return t1 ->
          MLTMatchWith (
            get_term_of_term deps sem t1,
            [
              (get_pattern deps sem p, get_term_of_skel deps sem s2) ;
              (MLPWild, MLTApp (MLTVar (["M"], "fail"), [MLTTop]))
            ]
          )
      | _ ->
          MLTLetStarIn (
            MLPVar !tmp,
            get_term_of_skel deps sem s1,
            MLTMatchWith (
              MLTVar ([], !tmp),
              [
                (get_pattern deps sem p, get_term_of_skel deps sem s2) ;
                (MLPWild, MLTApp (MLTVar (["M"], "fail"), [MLTTop]))
              ]
            )
          )
      end
  | LetIn (
    ( TermBind (_, (b, m), _, _) |
      SymbolBind (_, _, (b, m), _, _)), p, s1, s2) ->
      if is_exhaustive sem p then
        begin match s1.sdesc with
        | Return t1 ->
            (* bind (t1, λ p → s2) *)
            MLTApp ( MLTVar (of_from m, b),
              [MLTTuple ( get_term_of_term deps sem t1 ::
                MLTFunc (get_pattern deps sem p, get_term_of_skel deps sem s2) :: [])])
        | _ ->
            (* let* tmp = s1 in
               bind (tmp, λ p → s2) *)
            MLTLetStarIn (
              MLPVar !tmp, get_term_of_skel deps sem s1,
              MLTApp ( MLTVar (of_from m, b),
                [MLTTuple (MLTVar ([], !tmp) ::
                  MLTFunc (get_pattern deps sem p, get_term_of_skel deps sem s2) :: [])]
              )
            )
        end
      else
            (* let* tmp = s1 in
               bind (tmp, s')
               where s' = is the exhaustive version of [λ p → s2] *)
      MLTLetStarIn (
        MLPVar !tmp, get_term_of_skel deps sem s1,
        MLTApp ( MLTVar (of_from m, b),
          [MLTTuple (MLTVar ([], !tmp) ::
            get_term_of_term deps sem
            {tdesc=TFunc (p, s2);tloc=Location.ghost_loc;ttyp=
              {tydesc=Product [];tyloc=Location.ghost_loc}} :: [])]
        )
      )
  | Branching (_, outs) ->
      let branches =
        List.map (fun sk ->
          MLTFunc (MLPTop, get_term_of_skel deps sem sk)
        ) outs
      in
      MLTApp (MLTVar (["M"], "branch"), [MLTList branches])
  | Match ({sdesc=Return tm;_}, outs) ->
      let branches =
        List.map (fun (p, s) ->
          (get_pattern deps sem p, get_term_of_skel deps sem s)) outs
      in
      MLTMatchWith (get_term_of_term deps sem tm, branches)
  | Match (sm, outs) ->
      let branches =
        List.map (fun (p, s) ->
          (get_pattern deps sem p, get_term_of_skel deps sem s)) outs
      in
      (* let* _tmp = sm in … *)
      MLTLetStarIn (
        MLPVar !tmp, get_term_of_skel deps sem sm,
        MLTMatchWith (MLTVar ([], !tmp), branches))
  | Exists _ -> failwith "Existentials are not supported by Necro ML yet"
  end



(* Returns signature if it's polymorphic, None otherwise *)
let get_poly_sig ta (ty: typ) =
  if ta = [] then None else
  Some (MLTyPoly (ta, get_type ty))


(* Write the mutually-recursive terms *)
let get_term_definitions deps sem =
  let open SMap in
  let get_term_decl name (ta, ty, t, c) =
    (name, c, [], get_poly_sig ta ty, get_term_of_term deps sem t)
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
    begin match SMap.find_opt t spec_terms with
    | None -> assert false
    | Some x -> x
    end
  in
  let dep = Skeleton.term_dep_graph SMap.empty sem in
  let scc_all =
    let module SCC = Graph.Components.Make(Skeleton.PGraph) in
    SCC.scc_list dep
  in
  (* scc_all is a list of all strongly connected component of graph [dep],
     listed in topological order *)
  let scc =
    (* We filter out unspecified terms *)
    List.map (List.filter (fun (name, path) ->
      (path = None) &&
      begin match SMap.find_opt name sem.ss_terms with
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
  |> SMap.filter_map (fun _ (com, td) ->
      begin match td with TDUnspec i -> Some (com, i) | _ -> None end)
  |> SMap.bindings
  |> List.map (fun (x, (c, i)) -> type_with_comment (c, x, i))


(* Prints [val t: ty] for all unspecified terms *)
let get_term_signatures ~all sem =
  (* Prints the signature of a term *)
  let get_term_sig (name, (c, (_, ty, _))) =
    MTValDecl (c, name, get_type ty)
  in
  let all_sigs =
    sem.ss_terms
    |> (if all then Fun.id else SMap.filter (fun _ (_, (_, _, t)) -> t = None))
    |> SMap.bindings
  in
  List.map get_term_sig all_sigs

let get_terms_not_implemented deps sem =
  let get_term_not_implemented (name, _) =
    let error =
      (* raise NotImplemented "%name" *)
      MLTApp (
        MLTVar ([], "raise"),
        [ MLTConstr ([], "NotImplemented", Some (MLTString name)) ]
      )
    in
    MValDef (name, None, ["_"], None, error)
  in
  let terms =
    sem.ss_terms
    |> SMap.filter (fun _ (_, (_, ty, t)) ->
        begin match t, Skeleton.unalias deps sem ty with
        | None, {tydesc=Arrow _;_} -> true | _ -> false
        end)
    |> SMap.bindings
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
    begin match SMap.find_opt name ss.ss_types with
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
  let aliases = SMap.filter_map (fun _ (com, td) ->
    begin match td with
    | TDAlias (ta, ty) -> Some (com, ta, ty)
    | _ -> None
    end) ss.ss_types
  in
  let get_alias (name, (c, ta, ty)) =
    MLTDAlias (c, ta, name, get_type ty)
  in
  List.map get_alias (SMap.bindings aliases)

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
let unspec_module_type dep_map deps sem =
	let one_include (i, _) =
		let l =
			begin match SMap.find_opt i dep_map with
			| None -> assert false
			| Some l -> l
			end
		in
		let withes = (with_modules l) in
		MTModule (cap i, [cap i; "INTERPRETER"], withes)
	in
  let com = " All types, and the unspecified terms " in
  let def =
    MTBlock (
      MTModule ("M", ["MONAD"], []) ::
      List.map one_include (SMap.bindings deps) @
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
  List.map print_include (SMap.bindings sem.ss_deps)
*)

(*
let get_opens sem: module_tld list =
  let print_include (n, (_, op)) =
    if op then Some (MOpen [String.capitalize_ascii n]) else None
  in
  List.filter_map print_include (SMap.bindings sem.ss_deps)
*)

(* writes the MakeInterpreter functor *)
let makeinterpreter_functor deps sem =
  let comment = " Module defining the specified terms " in
  let args = ("F",["UNSPEC"],[]) ::
    List.map (fun (i, _) ->
      (cap i, [cap i;"INTERPRETER"], [])) (SMap.bindings deps)
  in
  let body =
    MInclude ["F"] ::
    MValDef ("( let* )", None, [], None, MLTVar (["M"], "bind")) ::
    get_term_definitions deps sem
  in
  Functor (Some comment, "MakeInterpreter", args, body)

let unspec_functor deps sem =
  let com = " A default instantiation " in
  let args =
    ("M", ["MONAD"], [])
    :: ("T", ["TYPES"], [])
    :: List.map (fun (i, _) ->
      (cap i, [cap i ; "INTERPRETER"], [("M",["M"])])) (SMap.bindings deps)
  in
  let body =
    MBlock (
      MException ("NotImplemented", MLTyString)
      :: MInclude ["T"]
      :: MModuleAlias ("M", ["M"])
      :: List.map (fun (i, _) ->
          MModuleAlias (cap i, [cap i])) (SMap.bindings deps))
    :: MTypeDefs (specified_types ~comm:false sem @ get_aliases sem)
    :: MBlock (get_terms_not_implemented deps sem)
    :: []
  in
  Functor (Some com, "Unspec", args, body)

let interpreter_module_type deps sem =
  let deps = List.map (fun (i, _) ->
    MTModule (cap i, [cap i; "INTERPRETER"], [("M",["M"])])) (SMap.bindings deps) in
  let comment = " The module type for interpreters " in
  let body =
    MTModule ("M", ["MONAD"], [])
    :: MTBlock deps
    :: MTBlock (unspec_type_decl sem)
    :: MTTypeDefs (specified_types ~comm:false sem @ get_aliases sem)
    :: MTBlock (get_term_signatures ~all:true sem)
    :: []
  in
  ModuleType (Some comment, "INTERPRETER", body)

let build_map deps sem =
	let rec aux map (i, s) =
		let incl_list = SSet.to_list s.ss_includes in
    let incl = List.map (fun i -> (i, SMap.find i deps)) incl_list in
		let map = SMap.add i incl_list map in
		List.fold_left aux map incl
	in
	aux SMap.empty ("this", sem)

(* writes the interpreter generator modules to stdout *)
let generate_interpreter (deps: skeletal_semantics SMap.t) (_self, sem): ast =
  let sem, _protect = Skeleton.protect_sem necro_ocaml_keywords sem in
  let version =
    begin match Build_info.V1.version () with
    | None -> "N/A"
    | Some v -> Build_info.V1.Version.to_string v
    end
  in
  let dep_map = build_map deps sem in
  let comm = Comment
  (" This file was automatically generated using necroml version " ^ version ^
  ".\nSee https://gitlab.inria.fr/skeletons/necro-ml/ for more informations. ") in
  let ast =
    [ comm
    ; types_module_type sem (* module type TYPES *)
    ; monad_module_type sem (* module type MONAD *)
    ; unspec_module_type dep_map deps sem (* module type UNSPEC *)
    ; unspec_functor deps sem (* module Unspec (M:MONAD) (T:TYPES) *)
    ; interpreter_module_type deps sem (* module type INTERPRETER *)
    ; makeinterpreter_functor deps sem (* module MakeInterpreter (F: UNSPEC) …*)
    ]
  in
  ast
