(** This file was automatically generated using necroml
	See https://gitlab.inria.fr/skeletons/necro-ml/ for more informations *)

(** The unspecified types *)
module type TYPES = sig
  type env
  type ident
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
  | Lam of (ident * lterm)
  | App of (lterm * lterm)
  and clos =
  | Clos of (ident * lterm * env)

  val extEnv: env * ident * clos -> env M.t
  val getEnv: ident * env -> clos M.t
end

(** A default instantiation *)
module Unspec (M: MONAD) (T: TYPES) = struct
  exception NotImplemented of string
  include T
  module M = M

  type lterm =
  | Var of ident
  | Lam of (ident * lterm)
  | App of (lterm * lterm)
  and clos =
  | Clos of (ident * lterm * env)

  let extEnv _ = raise (NotImplemented "extEnv")
  let getEnv _ = raise (NotImplemented "getEnv")
end

(** The module type for interpreters *)
module type INTERPRETER = sig
  module M: MONAD

  type env
  type ident

  type lterm =
  | Var of ident
  | Lam of (ident * lterm)
  | App of (lterm * lterm)
  and clos =
  | Clos of (ident * lterm * env)

  val eval: env -> (lterm -> clos M.t) M.t
  val extEnv: env * ident * clos -> env M.t
  val getEnv: ident * env -> clos M.t
end

(** Module defining the specified terms *)
module MakeInterpreter (F: UNSPEC) = struct
  include F

  let ( let* ) = M.bind

  let apply1 = M.apply
  let apply2 f arg1 arg2 =
    let* _tmp = apply1 f arg1 in
    apply1 _tmp arg2

  let rec eval s =
    M.ret (function l ->
      begin match l with
      | Lam (x, t) -> M.ret (Clos (x, t, s))
      | Var x -> apply1 getEnv (x, s)
      | App (t1, t2) ->
          let* Clos (x, t, s') = apply2 eval s t1 in
          let* w = apply2 eval s t2 in
          let* s'' = apply1 extEnv (s', x, w) in
          apply2 eval s'' t
      end)
end