(* necro language test : needs an interpreter to be generated in "necro.ml" *)

open Necro
open Util

module Flow = struct
  type nonrec string = string
  type 'a smap = 'a SMap.t

  type pterm =
  | TConstr of (string * pterm)
  | TTuple of (pterm) list
  | TVar of string
  and hook_decl =
  | HookSignature of (string * ptype * ptype)
  and pskeleton =
  | Branch of (pskeleton) list
  | Call of (string * pterm)
  | LetIn of (pterm * pskeleton * pskeleton)
  | Return of pterm
  and type_decl =
  | Abs of string
  | Var of (string * (constr_decl) list)
  and tterm =
  | TTerm of (tterm_desc * ttype)
  and decl =
  | Hook of hook_decl
  | Rule of rule_decl
  | Type of type_decl
  and ttree =
  | TTree of ((ttype) smap * (tsignature) smap * (thooks) smap)
  and tterm_desc =
  | TTConstr of (string * tterm)
  | TTTuple of (tterm) list
  | TTVar of (ttype * string)
  and ttype =
  | TBase of (kind * string)
  | TProd of (ttype) list
  and (_) option =
  | None
  | Some : 'a -> ('a) option
  and ptype =
  | Base of string
  | Prod of (ptype) list
  and trule =
  | TRule of (string * tterm * tskeleton)
  and tskeleton =
  | TBranch of (ttype * (tskeleton) list)
  | TCall of (string * kind * tterm * ttype)
  | TLetIn of (tterm * tskeleton * tskeleton)
  | TReturn of tterm
  and bool =
  | False
  | True
  and thooks =
  | THookDef of (tsignature * (trule) list)
  and ptree =
  | Ptree of (decl) list
  and tsignature =
  | TSig of (string * (ttype * ttype))
  and (_) list =
  | Cons : ('a * ('a) list) -> ('a) list
  | Nil
  and kind =
  | Abstract
  | Defined
  and typing_env =
  | TE of ((ttype) smap * (tsignature) smap * ((tsignature * kind)) smap)
  and rule_decl =
  | RuleDecl of (string * pterm * pskeleton)
  and constr_decl =
  | Constr of (string * ptype)

  let empty_smap () = SMap.empty
  let eq (x, y) =
    if x = y then True else False
  let merge2 (m1, m2) =
    let choose_first x a b = Option.Some a in
    SMap.union choose_first m1 m2
  let smap_add (x, y, m) = SMap.add x y m
  let smap_bindings m =
    let rec to_list = function
      | [] -> Nil
      | a :: q -> Cons (a, to_list q)
    in to_list (SMap.bindings m)
  let smap_get_opt (x, m) =
    let to_option = function
      | Option.Some a -> Some a
      | Option.None -> None
    in to_option (SMap.find_opt x m)
  let smap_keys m =
    let rec to_list = function
      | [] -> Nil
      | a :: q -> Cons (a, to_list q)
    in to_list (List.map fst (SMap.bindings m))
  let wild () = "_"
end

module InterpNecro : (INTERPRETER with
  type bool = Flow.bool and
  type constr_decl = Flow.constr_decl and
  type decl = Flow.decl and
  type hook_decl = Flow.hook_decl and
  type string = Flow.string and
  type kind = Flow.kind and
  type ('a) list = ('a) Flow.list and
  type ('a) smap = ('a) Flow.smap and
  type ('a) option = ('a) Flow.option and
  type pskeleton = Flow.pskeleton and
  type pterm = Flow.pterm and
  type ptree = Flow.ptree and
  type ptype = Flow.ptype and
  type rule_decl = Flow.rule_decl and
  type thooks = Flow.thooks and
  type trule = Flow.trule and
  type tsignature = Flow.tsignature and
  type tskeleton = Flow.tskeleton and
  type tterm = Flow.tterm and
  type ttree = Flow.ttree and
  type ttype = Flow.ttype and
  type type_decl = Flow.type_decl and
  type typing_env = Flow.typing_env) = MakeInterpreter (Flow)

open Flow
open InterpNecro

let () =
  let empty = Ptree Nil in
  let skeletal_semantics = type_tree empty in
  let TTree (m1, m2, m3) = skeletal_semantics in
  let () = assert(SMap.is_empty m1) in
  let () = assert(SMap.is_empty m2) in
  let () = assert(SMap.is_empty m3) in
  ()

let rec to_necro_list = function
  | [] -> Nil
  | a :: q -> Cons (a, to_necro_list q)

let rec of_necro_list = function
  | Nil -> []
  | Cons (a, q) -> a :: (of_necro_list q)




open Skeltypes
open Util

(* Casting from Flow.ttree to Skeltypes.skeletal_semantics. These are
 * only trivial transformations *)
let rec kind_to_sort_kind = function
  | Flow.Abstract -> Skeltypes.Abstract
  | Flow.Defined -> Skeltypes.Variant

let rec kind_to_bool = function
  | Flow.Abstract -> true
  | Flow.Defined -> false

let rec ttype_to_sort = function
  | TBase (k, t) ->
      let s_kind = kind_to_sort_kind k in
      let s_name = t in
      let s_args = [] in
      Sort { s_kind ; s_name ; s_args }
  | TProd tl -> Product (List.map ttype_to_sort (of_necro_list tl))

let rec tterm_desc_to_term_desc = function
  | TTVar (ty, s) ->
      let tv_name = s in
      let tv_typ = ttype_to_sort ty in
      TVar { tv_name; tv_typ }
  | TTTuple tl ->
      TTuple (List.map tterm_to_term (of_necro_list tl))
  | TTConstr (c, t) ->
      TConstr (c, [], tterm_to_term t)

and tterm_to_term = function
  | TTerm (td, ty) ->
      let t_desc = tterm_desc_to_term_desc td in
      let t_typ = ttype_to_sort ty in
      { t_desc ; t_typ }

let rec tskeleton_to_skeleton = function
  | TBranch (ty, skl) ->
      let b_branches = List.map tskeleton_to_skeleton (of_necro_list skl) in
      let b_output_sort = ttype_to_sort ty in
      Branching { b_branches ; b_output_sort }
  | TCall (h, k, t, out) ->
      let c_abstr = kind_to_bool k in
      let c_name = h in
      let c_annot = "" in
      let c_input = tterm_to_term t in
      let c_output_sort = ttype_to_sort out in
      let c_type_args = [] in
      Hook { c_abstr ; c_name ; c_annot ; c_input ; c_output_sort ; c_type_args }
  | TLetIn (t, s1, s2) ->
      LetIn (None, tterm_to_term t, tskeleton_to_skeleton s1, tskeleton_to_skeleton s2)
  | TReturn t -> Return (None, tterm_to_term t)

let rec tsignature_to_constructor_signature = function
  | TSig (c, (in_type, out_type)) ->
      let cs_name = c in
      let cs_type_args = [] in
      let cs_input_sort = ttype_to_sort in_type in
      let cs_output_sort = ttype_to_sort out_type in
      { cs_name ; cs_type_args ; cs_input_sort ; cs_output_sort }

let rec tsignature_to_hook_signature = function
  | TSig (h, (in_type, out_type)) ->
      let hs_name = h in
      let hs_type_args = [] in
      let hs_input_sort = ttype_to_sort in_type in
      let hs_output_sort = ttype_to_sort out_type in
      { hs_name ; hs_type_args ; hs_input_sort ; hs_output_sort }

let rec trule_to_rule hs = function
  | TRule (h, t, sk) ->
      let r_hook = h in
      let r_input = tterm_to_term t in
      let r_skeleton = tskeleton_to_skeleton sk in
      let r_type_args = [] in
      { r_hook ; r_input ; r_skeleton ; r_type_args }

let rec thooks_to_hook_signature_prod_rule_list = function
  | THookDef (hsig, rl) ->
      let hsig = tsignature_to_hook_signature hsig in
      let rl = List.map (trule_to_rule hsig) (of_necro_list rl) in
      (hsig, rl)

let first_map_to_ss_sorts m1 =
  SMap.map ttype_to_sort m1

let snd_map_to_ss_constr m2 =
  SMap.map tsignature_to_constructor_signature m2

let third_map_to_ss_hooks m3 =
  SMap.map thooks_to_hook_signature_prod_rule_list m3

let ttree_to_skeletal_semantics: ttree -> skeletal_semantics = function
  | TTree (m1, m2, m3) ->
      let ss_sorts = first_map_to_ss_sorts m1 in
      let ss_constr_signatures = snd_map_to_ss_constr m2 in
      let ss_hooks = third_map_to_ss_hooks m3 in
      let ss_monads = SMap.empty in
      let ss_terms = SMap.empty in
      { ss_sorts ; ss_constr_signatures ; ss_hooks ; ss_monads ; ss_terms }




(* Casting from Pskeltypes.pdecls (assuming there is neither polymorphism nor
 * monads nor functions) to Flow.ptree. These are once again only trivial
 * transformations *)
open Pskeltypes

let rec pterm_to_pterm t =
  match t.contents with
  | PTVar v -> Flow.TVar v
  | PTConstr (c, l, t) ->
      let () = assert (l = []) in
      Flow.TConstr (c.contents, pterm_to_pterm t)
  | PTType (_, t) -> pterm_to_pterm t
  | PTTuple tl -> Flow.TTuple (to_necro_list (List.map pterm_to_pterm tl))
  | PTFunc _ -> assert false (* No functions *)

let rec poly_psort_to_ptype = function
  | Pskeltypes.Psort (s, l) ->
      let () = assert (l = []) in
      Base s
  | Pskeltypes.Prod l ->
      let l = List.map (fun s -> s.contents) l in
      Prod (to_necro_list (List.map poly_psort_to_ptype l))
  | Pskeltypes.Arrow _ -> assert false (* No functions *)

let pabs_type_to_decl (s, i) =
  let () = assert (i = 0) in (* No poly *)
  Type (Abs s)

let pvar_type_to_decl (s, l, cl) =
  let () = assert (l = []) in (* No poly *)
  let constr (c, ta, s) =
    let () = assert (ta = []) in (* No poly *)
    Constr (c.contents, poly_psort_to_ptype s.contents)
  in
  Type (Var (s, to_necro_list (List.map constr cl)))

let phook_decl_to_decl (h, l, t_in, t_out) =
  let () = assert (l = []) in (* No poly *)
  let t_in = poly_psort_to_ptype t_in.contents in
  let t_out = poly_psort_to_ptype t_out.contents in
  Flow.Hook (HookSignature (h, t_in, t_out))

let rec pskel_to_pskel = function
  | PCall pc ->
      let () = assert (pc.pc_type_args = []) in (* No poly *)
      Call (pc.pc_name.contents, pterm_to_pterm pc.pc_input)
  | PBranching sl ->
      Branch (to_necro_list (List.map (fun x -> pskel_to_pskel x.contents) sl))
  | PReturn (a, t) ->
      let () = assert (a = None) in (* No poly *)
      Return (pterm_to_pterm t)
  | PLetIn (a, t, s1, s2) ->
      let () = assert (a = None) in (* No poly *)
      LetIn (pterm_to_pterm t, pskel_to_pskel s1.contents, pskel_to_pskel s2.contents)

let prule_def_to_decl r =
  let pterm = pterm_to_pterm r.pr_input in
  let skel = pskel_to_pskel r.pr_skeleton.contents in
  Rule (RuleDecl (r.pr_name.contents, pterm, skel))

let pdecls_to_ptree (d: Pskeltypes.pdecls) : Flow.ptree =
  let () = assert ([] = d.pmonads) in (* No monad *)
  let l1 = List.map pabs_type_to_decl d.pabs_type in
  let l2 = List.map pvar_type_to_decl d.pvar_type in
  let l3 = List.map phook_decl_to_decl d.phook_decl in
  let l4 = List.map prule_def_to_decl d.prule_def in
  Ptree (to_necro_list (l1 @ l2 @ l3 @ l4))



let check_file f =
  let parsed = Parserutil.parse_from_file Parser.main f in
  let sem1 = Typer.type_program f parsed [] in
  let sem2 = ttree_to_skeletal_semantics (type_tree (pdecls_to_ptree parsed)) in
  assert(Skeleton.same_ss sem1 sem2)

let check_and_print_file f =
  let parsed = Parserutil.parse_from_file Parser.main f in
  let sem1 = Typer.type_program f parsed [] in
  let sem2 = ttree_to_skeletal_semantics (type_tree (pdecls_to_ptree parsed)) in
  print_endline (Printer.string_of_ss sem2);
  assert(Skeleton.same_ss sem1 sem2)


let () =
  let () = check_file "examples/arithmetic.sk" in
  let () = check_file "examples/concurrent_ss.sk" in
  let () = check_file "examples/implicits.sk" in
  let () = check_file "examples/lambda.sk" in
  let () = check_file "examples/nat.sk" in
  let () = check_file "examples/pcf.sk" in
  let () = check_file "examples/stack.sk" in
  let () = check_file "examples/typed_cbn_lambda.sk" in
  let () = check_file "examples/while.sk" in
  ()


