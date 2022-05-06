(** This file was automatically generated using necroml
	See https://gitlab.inria.fr/skeletons/necro-ml/ for more informations *)

(** The unspecified types *)
module type TYPES = sig
  type env
  type ident
  type int
  type lit
  type value
end

(** The interpretation monad *)
module type MONAD = sig
  type 'a t
  val ret: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val branch: (unit -> 'a t) list -> 'a t
  val fail: string -> 'a t
  val apply: ('a -> 'b t) -> 'a -> 'b t
  val extract: 'a t -> 'a
end

(** All types, and the unspecified terms *)
module type UNSPEC = sig
  module M: MONAD
  include TYPES

  type pattern =
  | PWildcard
  | PVar of ident
  | POr of (pattern * pattern)
  | PConstr of (ident * pattern list)
  and 'a option =
  | Some of 'a
  | None
  and mlterm =
  | Var of ident
  | Plus of (mlterm * mlterm)
  | Match of (mlterm * (pattern * mlterm) list)
  | Lam of (ident * mlterm)
  | IfZero of (mlterm * mlterm * mlterm)
  | Constr of (ident * mlterm list)
  | Const of lit
  | App of (mlterm * mlterm)
  and 'a list =
  | Nil
  | Cons of ('a * 'a list)

  val extEnv: env * ident * value -> env M.t
  val getClos: value -> (ident * mlterm * env) M.t
  val getEnv: ident * env -> value M.t
  val getInt: value -> int M.t
  val intOfLit: lit -> int M.t
  val isConstrWithName: ident * value -> (value list) M.t
  val isNotConstrWithName: ident * value -> unit M.t
  val isNotZero: int -> unit M.t
  val isZero: int -> unit M.t
  val match_failure: unit -> value M.t
  val mkClos: ident * mlterm * env -> value M.t
  val mkConstr: ident * value list -> value M.t
  val mkInt: int -> value M.t
  val plus: int * int -> int M.t
end

(** A default instantiation *)
module Unspec (M: MONAD) (T: TYPES) = struct
  exception NotImplemented of string
  include T
  module M = M

  type pattern =
  | PWildcard
  | PVar of ident
  | POr of (pattern * pattern)
  | PConstr of (ident * pattern list)
  and 'a option =
  | Some of 'a
  | None
  and mlterm =
  | Var of ident
  | Plus of (mlterm * mlterm)
  | Match of (mlterm * (pattern * mlterm) list)
  | Lam of (ident * mlterm)
  | IfZero of (mlterm * mlterm * mlterm)
  | Constr of (ident * mlterm list)
  | Const of lit
  | App of (mlterm * mlterm)
  and 'a list =
  | Nil
  | Cons of ('a * 'a list)

  let extEnv _ = raise (NotImplemented "extEnv")
  let getClos _ = raise (NotImplemented "getClos")
  let getEnv _ = raise (NotImplemented "getEnv")
  let getInt _ = raise (NotImplemented "getInt")
  let intOfLit _ = raise (NotImplemented "intOfLit")
  let isConstrWithName _ = raise (NotImplemented "isConstrWithName")
  let isNotConstrWithName _ = raise (NotImplemented "isNotConstrWithName")
  let isNotZero _ = raise (NotImplemented "isNotZero")
  let isZero _ = raise (NotImplemented "isZero")
  let match_failure _ = raise (NotImplemented "match_failure")
  let mkClos _ = raise (NotImplemented "mkClos")
  let mkConstr _ = raise (NotImplemented "mkConstr")
  let mkInt _ = raise (NotImplemented "mkInt")
  let plus _ = raise (NotImplemented "plus")
end

(** The module type for interpreters *)
module type INTERPRETER = sig
  module M: MONAD

  type env
  type ident
  type int
  type lit
  type value

  type pattern =
  | PWildcard
  | PVar of ident
  | POr of (pattern * pattern)
  | PConstr of (ident * pattern list)
  and 'a option =
  | Some of 'a
  | None
  and mlterm =
  | Var of ident
  | Plus of (mlterm * mlterm)
  | Match of (mlterm * (pattern * mlterm) list)
  | Lam of (ident * mlterm)
  | IfZero of (mlterm * mlterm * mlterm)
  | Constr of (ident * mlterm list)
  | Const of lit
  | App of (mlterm * mlterm)
  and 'a list =
  | Nil
  | Cons of ('a * 'a list)

  val eval: env * mlterm -> value M.t
  val eval_list: env * mlterm list -> (value list) M.t
  val eval_pattern: env * value * pattern -> (env option) M.t
  val extEnv: env * ident * value -> env M.t
  val getClos: value -> (ident * mlterm * env) M.t
  val getEnv: ident * env -> value M.t
  val getInt: value -> int M.t
  val intOfLit: lit -> int M.t
  val isConstrWithName: ident * value -> (value list) M.t
  val isNotConstrWithName: ident * value -> unit M.t
  val isNotZero: int -> unit M.t
  val isZero: int -> unit M.t
  val match_failure: unit -> value M.t
  val mkClos: ident * mlterm * env -> value M.t
  val mkConstr: ident * value list -> value M.t
  val mkInt: int -> value M.t
  val mlmatching: env * value * (pattern * mlterm) list -> value M.t
  val opt: 'a option -> (('a -> ('b option) M.t) -> ('b option) M.t) M.t
  val pattern_list: env * value list * pattern list -> (env option) M.t
  val plus: int * int -> int M.t
end

(** Module defining the specified terms *)
module MakeInterpreter (F: UNSPEC) = struct
  include F

  let ( let* ) = M.bind

  let apply1 = M.apply
  let apply2 f arg1 arg2 =
    let* _tmp = apply1 f arg1 in
    apply1 _tmp arg2

  let rec eval =
    function (env, t) ->
    M.branch [
      (function () ->
        begin match t with
        | Lam (x, body) -> apply1 mkClos (x, body, env)
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match t with
        | Var v -> apply1 getEnv (v, env)
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match t with
        | App (t1, t2) ->
            let* f = apply1 eval (env, t1) in
            let* (x, body, env1) = apply1 getClos f in
            let* v = apply1 eval (env, t2) in
            let* env2 = apply1 extEnv (env1, x, v) in
            apply1 eval (env2, body)
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match t with
        | Const n ->
            let* x = apply1 intOfLit n in
            apply1 mkInt x
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match t with
        | Plus (t1, t2) ->
            let* v1 = apply1 eval (env, t1) in
            let* x1 = apply1 getInt v1 in
            let* v2 = apply1 eval (env, t2) in
            let* x2 = apply1 getInt v2 in
            let* x = apply1 plus (x1, x2) in
            apply1 mkInt x
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match t with
        | IfZero (cond, ifz, ifnz) ->
            let* v = apply1 eval (env, cond) in
            let* x = apply1 getInt v in
            M.branch [
              (function () ->
                let* _ = apply1 isZero x in
                apply1 eval (env, ifz)) ;
              (function () ->
                let* _ = apply1 isNotZero x in
                apply1 eval (env, ifnz))]
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match t with
        | Constr (name, tl) ->
            let* vl = apply1 eval_list (env, tl) in
            apply1 mkConstr (name, vl)
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match t with
        | Match (tm, m) ->
            let* v = apply1 eval (env, tm) in
            apply1 mlmatching (env, v, m)
        | _ -> M.fail ""
        end)]
  and eval_list =
    function (env, t) ->
    M.branch [
      (function () ->
        begin match t with
        | Nil -> M.ret Nil
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match t with
        | Cons (t, tl) ->
            let* v = apply1 eval (env, t) in
            let* vl = apply1 eval_list (env, tl) in
            M.ret (Cons (v, vl))
        | _ -> M.fail ""
        end)]
  and eval_pattern =
    function (env, v, p) ->
    M.branch [
      (function () ->
        begin match p with
        | PWildcard -> M.ret (Some env)
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match p with
        | PVar x ->
            let* env1 = apply1 extEnv (env, x, v) in
            M.ret (Some env1)
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match p with
        | POr (p1, p2) ->
            let* env1opt = apply1 eval_pattern (env, v, p1) in
            M.branch [
              (function () ->
                begin match env1opt with
                | Some env1 -> M.ret (Some env1)
                | _ -> M.fail ""
                end) ;
              (function () ->
                begin match env1opt with
                | None -> apply1 eval_pattern (env, v, p2)
                | _ -> M.fail ""
                end)]
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match p with
        | PConstr (name, pl) ->
            M.branch [
              (function () ->
                let* args = apply1 isConstrWithName (name, v) in
                apply1 pattern_list (env, args, pl)) ;
              (function () ->
                let* _ = apply1 isNotConstrWithName (name, v) in
                M.ret None)]
        | _ -> M.fail ""
        end)]
  and mlmatching =
    function (env, v, l) ->
    M.branch [
      (function () ->
        begin match l with
        | Nil -> apply1 match_failure ()
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match l with
        | Cons (x, m) ->
            let (p, t) = x in
            let* env1opt = apply1 eval_pattern (env, v, p) in
            M.branch [
              (function () ->
                begin match env1opt with
                | Some env1 -> apply1 eval (env1, t)
                | _ -> M.fail ""
                end) ;
              (function () ->
                begin match env1opt with
                | None -> apply1 mlmatching (env, v, m)
                | _ -> M.fail ""
                end)]
        | _ -> M.fail ""
        end)]
  and opt: 'a 'b. 'a option -> (('a -> ('b option) M.t) -> ('b option) M.t) M.t  =
    function o ->
    M.ret (function f ->
      M.branch [
        (function () ->
          begin match o with
          | None -> M.ret None
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match o with
          | Some x -> apply1 f x
          | _ -> M.fail ""
          end)])
  and pattern_list =
    function (env, lv, lp) ->
    M.branch [
      (function () ->
        begin match (lv, lp) with
        | (Nil, Nil) -> M.ret (Some env)
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match (lv, lp) with
        | (Cons _, Nil) -> M.ret None
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match (lv, lp) with
        | (Nil, Cons _) -> M.ret None
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match (lv, lp) with
        | (Cons (v, t), Cons (p, pl)) ->
            let* _tmp = apply1 eval_pattern (env, v, p) in
            apply2 opt _tmp (function env1 ->
              apply1 pattern_list (env1, t, pl))
        | _ -> M.fail ""
        end)]
end