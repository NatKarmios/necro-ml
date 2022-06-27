(** This file was automatically generated using necroml
	See https://gitlab.inria.fr/skeletons/necro-ml/ for more informations *)

(** The unspecified types *)
module type TYPES = sig
  type ident
  type lit
  type state
  type value
  type vbool
  type vint
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

  type stmt =
  | While2 of (expr * expr * stmt)
  | While of (expr * stmt)
  | Skip
  | Seq of (stmt * stmt)
  | Parallel of (stmt * stmt)
  | If of (expr * stmt * stmt)
  | Assign of (ident * expr)
  | Assert of expr
  and expr =
  | Var of ident
  | Value of value
  | Plus of (expr * expr)
  | Not of expr
  | Equal of (expr * expr)
  | Const of lit

  val add: vint * vint -> value M.t
  val eq: vint * vint -> value M.t
  val isBool: value -> vbool M.t
  val isFalse: vbool -> unit M.t
  val isInt: value -> vint M.t
  val isTrue: vbool -> unit M.t
  val litToVal: lit -> value M.t
  val neg: vbool -> value M.t
  val read: ident * state -> value M.t
  val write: ident * state * value -> state M.t
end

(** A default instantiation *)
module Unspec (M: MONAD) (T: TYPES) = struct
  exception NotImplemented of string
  include T
  module M = M

  type stmt =
  | While2 of (expr * expr * stmt)
  | While of (expr * stmt)
  | Skip
  | Seq of (stmt * stmt)
  | Parallel of (stmt * stmt)
  | If of (expr * stmt * stmt)
  | Assign of (ident * expr)
  | Assert of expr
  and expr =
  | Var of ident
  | Value of value
  | Plus of (expr * expr)
  | Not of expr
  | Equal of (expr * expr)
  | Const of lit

  let add _ = raise (NotImplemented "add")
  let eq _ = raise (NotImplemented "eq")
  let isBool _ = raise (NotImplemented "isBool")
  let isFalse _ = raise (NotImplemented "isFalse")
  let isInt _ = raise (NotImplemented "isInt")
  let isTrue _ = raise (NotImplemented "isTrue")
  let litToVal _ = raise (NotImplemented "litToVal")
  let neg _ = raise (NotImplemented "neg")
  let read _ = raise (NotImplemented "read")
  let write _ = raise (NotImplemented "write")
end

(** The module type for interpreters *)
module type INTERPRETER = sig
  module M: MONAD

  type ident
  type lit
  type state
  type value
  type vbool
  type vint

  type stmt =
  | While2 of (expr * expr * stmt)
  | While of (expr * stmt)
  | Skip
  | Seq of (stmt * stmt)
  | Parallel of (stmt * stmt)
  | If of (expr * stmt * stmt)
  | Assign of (ident * expr)
  | Assert of expr
  and expr =
  | Var of ident
  | Value of value
  | Plus of (expr * expr)
  | Not of expr
  | Equal of (expr * expr)
  | Const of lit

  val add: vint * vint -> value M.t
  val eq: vint * vint -> value M.t
  val eval_expr: state -> (expr -> expr M.t) M.t
  val eval_stmt: state -> (stmt -> state M.t) M.t
  val evalss_stmt: state -> (stmt -> (state * stmt) M.t) M.t
  val isBool: value -> vbool M.t
  val isFalse: vbool -> unit M.t
  val isInt: value -> vint M.t
  val isTrue: vbool -> unit M.t
  val litToVal: lit -> value M.t
  val neg: vbool -> value M.t
  val read: ident * state -> value M.t
  val write: ident * state * value -> state M.t
end

(** Module defining the specified terms *)
module MakeInterpreter (F: UNSPEC) = struct
  include F

  let ( let* ) = M.bind

  let apply1 = M.apply
  let apply2 f arg1 arg2 =
    let* _tmp = apply1 f arg1 in
    apply1 _tmp arg2

  let rec eval_expr s =
    M.ret (function e ->
      begin match e with
      | Const i ->
          let* v = apply1 litToVal i in
          M.ret (Value v)
      | Var x ->
          let* v = apply1 read (x, s) in
          M.ret (Value v)
      | Plus (Value v1, Value v2) ->
          let* i1 = apply1 isInt v1 in
          let* i2 = apply1 isInt v2 in
          let* v = apply1 add (i1, i2) in
          M.ret (Value v)
      | Plus (t1, t2) ->
          M.branch [
            (function () ->
              let* t'1 = apply2 eval_expr s t1 in
              M.ret (Plus (t'1, t2))) ;
            (function () ->
              let* t'2 = apply2 eval_expr s t2 in
              M.ret (Plus (t1, t'2)))]
      | Equal (Value v1, Value v2) ->
          let* i1 = apply1 isInt v1 in
          let* i2 = apply1 isInt v2 in
          let* v = apply1 eq (i1, i2) in
          M.ret (Value v)
      | Equal (t1, t2) ->
          M.branch [
            (function () ->
              let* t'1 = apply2 eval_expr s t1 in
              M.ret (Equal (t'1, t2))) ;
            (function () ->
              let* t'2 = apply2 eval_expr s t2 in
              M.ret (Equal (t1, t'2)))]
      | Not Value v ->
          let* b = apply1 isBool v in
          let* b' = apply1 neg b in
          M.ret (Value b')
      | Not t ->
          let* t' = apply2 eval_expr s t in
          M.ret (Not t')
      | Value _ -> M.branch []
      end)
  and eval_stmt s =
    M.ret (function t ->
      begin match t with
      | Skip -> M.ret s
      | _ ->
          let* (s, t) = apply2 evalss_stmt s t in
          apply2 eval_stmt s t
      end)
  and evalss_stmt s =
    M.ret (function t ->
      begin match t with
      | Assign (x, Value v) ->
          let* s' = apply1 write (x, s, v) in
          M.ret (s', Skip)
      | Assign (x, t) ->
          let* t' = apply2 eval_expr s t in
          M.ret (s, Assign (x, t'))
      | Seq (Skip, t2) -> M.ret (s, t2)
      | Seq (t1, t2) ->
          let* (s', t'1) = apply2 evalss_stmt s t1 in
          M.ret (s', Seq (t'1, t2))
      | If (Value e, iftrue, iffalse) ->
          let* b = apply1 isBool e in
          M.branch [
            (function () ->
              let* _ = apply1 isTrue b in
              M.ret (s, iftrue)) ;
            (function () ->
              let* _ = apply1 isFalse b in
              M.ret (s, iffalse))]
      | If (e, iftrue, iffalse) ->
          let* e' = apply2 eval_expr s e in
          M.ret (s, If (e', iftrue, iffalse))
      | While (e, t) -> M.ret (s, While2 (e, e, t))
      | While2 (Value v, e, t) ->
          let* b = apply1 isBool v in
          M.branch [
            (function () ->
              let* _ = apply1 isTrue b in
              M.ret (s, Seq (t, While (e, t)))) ;
            (function () ->
              let* _ = apply1 isFalse b in
              M.ret (s, Skip))]
      | While2 (e1, e2, t) ->
          let* e'1 = apply2 eval_expr s e1 in
          M.ret (s, While2 (e'1, e2, t))
      | Parallel (Skip, Skip) -> M.ret (s, Skip)
      | Parallel (t1, t2) ->
          M.branch [
            (function () ->
              let* (s', t'1) = apply2 evalss_stmt s t1 in
              M.ret (s', Parallel (t'1, t2))) ;
            (function () ->
              let* (s', t'2) = apply2 evalss_stmt s t2 in
              M.ret (s', Parallel (t1, t'2)))]
      | Assert Value v ->
          let* b = apply1 isBool v in
          let* () = apply1 isTrue b in
          M.ret (s, Skip)
      | Assert e ->
          let* e' = apply2 eval_expr s e in
          M.ret (s, Assert e')
      | Skip -> M.branch []
      end)
end