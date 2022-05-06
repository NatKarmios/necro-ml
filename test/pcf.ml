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

  type lterm =
  | Var of ident
  | Plus of (lterm * lterm)
  | Lam of (ident * lterm)
  | IfZero of (lterm * lterm * lterm)
  | Const of lit
  | App of (lterm * lterm)

  val extEnv: env * ident * value -> env M.t
  val getClos: value -> (ident * lterm * env) M.t
  val getEnv: ident * env -> value M.t
  val getInt: value -> int M.t
  val intOfLit: lit -> int M.t
  val isNotZero: int -> unit M.t
  val isZero: int -> unit M.t
  val mkClos: ident * lterm * env -> value M.t
  val mkInt: int -> value M.t
  val plus: int * int -> int M.t
end

(** A default instantiation *)
module Unspec (M: MONAD) (T: TYPES) = struct
  exception NotImplemented of string
  include T
  module M = M

  type lterm =
  | Var of ident
  | Plus of (lterm * lterm)
  | Lam of (ident * lterm)
  | IfZero of (lterm * lterm * lterm)
  | Const of lit
  | App of (lterm * lterm)

  let extEnv _ = raise (NotImplemented "extEnv")
  let getClos _ = raise (NotImplemented "getClos")
  let getEnv _ = raise (NotImplemented "getEnv")
  let getInt _ = raise (NotImplemented "getInt")
  let intOfLit _ = raise (NotImplemented "intOfLit")
  let isNotZero _ = raise (NotImplemented "isNotZero")
  let isZero _ = raise (NotImplemented "isZero")
  let mkClos _ = raise (NotImplemented "mkClos")
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

  type lterm =
  | Var of ident
  | Plus of (lterm * lterm)
  | Lam of (ident * lterm)
  | IfZero of (lterm * lterm * lterm)
  | Const of lit
  | App of (lterm * lterm)

  val eval: env * lterm -> value M.t
  val extEnv: env * ident * value -> env M.t
  val getClos: value -> (ident * lterm * env) M.t
  val getEnv: ident * env -> value M.t
  val getInt: value -> int M.t
  val intOfLit: lit -> int M.t
  val isNotZero: int -> unit M.t
  val isZero: int -> unit M.t
  val mkClos: ident * lterm * env -> value M.t
  val mkInt: int -> value M.t
  val plus: int * int -> int M.t
end

(** Module defining the specified terms *)
module MakeInterpreter (F: UNSPEC) = struct
  include F

  let ( let* ) = M.bind

  let apply1 = M.apply

  let rec eval =
    function (s, t) ->
    M.branch [
      (function () ->
        begin match t with
        | Lam (x, body) -> apply1 mkClos (x, body, s)
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match t with
        | Var v -> apply1 getEnv (v, s)
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match t with
        | App (t1, t2) ->
            let* f = apply1 eval (s, t1) in
            let* (x, body, env) = apply1 getClos f in
            let* v = apply1 eval (s, t2) in
            let* nenv = apply1 extEnv (env, x, v) in
            apply1 eval (nenv, body)
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
            let* v1 = apply1 eval (s, t1) in
            let* x1 = apply1 getInt v1 in
            let* v2 = apply1 eval (s, t2) in
            let* x2 = apply1 getInt v2 in
            let* x = apply1 plus (x1, x2) in
            apply1 mkInt x
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match t with
        | IfZero (cond, ifz, ifnz) ->
            let* v = apply1 eval (s, cond) in
            let* x = apply1 getInt v in
            M.branch [
              (function () ->
                let* _ = apply1 isZero x in
                apply1 eval (s, ifz)) ;
              (function () ->
                let* _ = apply1 isNotZero x in
                apply1 eval (s, ifnz))]
        | _ -> M.fail ""
        end)]
end