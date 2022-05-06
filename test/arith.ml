(** This file was automatically generated using necroml
	See https://gitlab.inria.fr/skeletons/necro-ml/ for more informations *)

(** The unspecified types *)
module type TYPES = sig
  type literal
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

  type expr =
  | Const of literal
  | Binop of (binop * expr * expr)
  and binop =
  | Sub
  | Mult
  | Div
  | Add

  val add: value -> (value -> value M.t) M.t
  val div: value -> (value -> value M.t) M.t
  val litToVal: literal -> value M.t
  val mult: value -> (value -> value M.t) M.t
  val sub: value -> (value -> value M.t) M.t
end

(** A default instantiation *)
module Unspec (M: MONAD) (T: TYPES) = struct
  exception NotImplemented of string
  include T
  module M = M

  type expr =
  | Const of literal
  | Binop of (binop * expr * expr)
  and binop =
  | Sub
  | Mult
  | Div
  | Add

  let add _ = raise (NotImplemented "add")
  let div _ = raise (NotImplemented "div")
  let litToVal _ = raise (NotImplemented "litToVal")
  let mult _ = raise (NotImplemented "mult")
  let sub _ = raise (NotImplemented "sub")
end

(** The module type for interpreters *)
module type INTERPRETER = sig
  module M: MONAD

  type literal
  type value

  type expr =
  | Const of literal
  | Binop of (binop * expr * expr)
  and binop =
  | Sub
  | Mult
  | Div
  | Add

  val add: value -> (value -> value M.t) M.t
  val apply: binop -> (value -> (value -> value M.t) M.t) M.t
  val div: value -> (value -> value M.t) M.t
  val eval: expr -> value M.t
  val litToVal: literal -> value M.t
  val mult: value -> (value -> value M.t) M.t
  val sub: value -> (value -> value M.t) M.t
end

(** Module defining the specified terms *)
module MakeInterpreter (F: UNSPEC) = struct
  include F

  let ( let* ) = M.bind

  let apply1 = M.apply
  let apply2 f arg1 arg2 =
    let* _tmp = apply1 f arg1 in
    apply1 _tmp arg2
  let apply3 f arg1 arg2 arg3 =
    let* _tmp = apply1 f arg1 in
    apply2 _tmp arg2 arg3

  let rec apply b =
    begin match b with
    | Add -> M.ret add
    | Div -> M.ret div
    | Mult -> M.ret mult
    | Sub -> M.ret sub
    end
  and eval e =
    begin match e with
    | Const t -> apply1 litToVal t
    | Binop (op, t1, t2) ->
        let* f1 = apply1 eval t1 in
        let* f2 = apply1 eval t2 in
        apply3 apply op f1 f2
    end
end