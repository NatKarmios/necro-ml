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
  | While of (expr * stmt)
  | Skip
  | Seq of (stmt * stmt)
  | If of (expr * stmt * stmt)
  | Assign of (ident * expr)
  and expr =
  | Var of ident
  | Plus of (expr * expr)
  | Not of expr
  | Equal of (expr * expr)
  | Const of lit

  val add: vint * vint -> vint M.t
  val eq: vint * vint -> unit M.t
  val false': vbool
  val isBool: value -> vbool M.t
  val isFalse: vbool -> unit M.t
  val isInt: value -> vint M.t
  val isTrue: vbool -> unit M.t
  val litToVal: lit -> value M.t
  val mkBool: vbool -> value M.t
  val mkInt: vint -> value M.t
  val neq: vint * vint -> unit M.t
  val one: vint
  val read: ident * state -> value M.t
  val true': vbool
  val write: ident * state * value -> state M.t
  val zero: vint
end

(** A default instantiation *)
module Unspec (M: MONAD) (T: TYPES) = struct
  exception NotImplemented of string
  include T
  module M = M

  type stmt =
  | While of (expr * stmt)
  | Skip
  | Seq of (stmt * stmt)
  | If of (expr * stmt * stmt)
  | Assign of (ident * expr)
  and expr =
  | Var of ident
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
  let mkBool _ = raise (NotImplemented "mkBool")
  let mkInt _ = raise (NotImplemented "mkInt")
  let neq _ = raise (NotImplemented "neq")
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
  | While of (expr * stmt)
  | Skip
  | Seq of (stmt * stmt)
  | If of (expr * stmt * stmt)
  | Assign of (ident * expr)
  and expr =
  | Var of ident
  | Plus of (expr * expr)
  | Not of expr
  | Equal of (expr * expr)
  | Const of lit

  val add: vint * vint -> vint M.t
  val eq: vint * vint -> unit M.t
  val eqBoolean: vbool -> (vbool -> vbool M.t) M.t
  val eval_expr: state -> (expr -> value M.t) M.t
  val eval_stmt: state -> (stmt -> state M.t) M.t
  val false': vbool
  val isBool: value -> vbool M.t
  val isFalse: vbool -> unit M.t
  val isInt: value -> vint M.t
  val isTrue: vbool -> unit M.t
  val litToVal: lit -> value M.t
  val mkBool: vbool -> value M.t
  val mkInt: vint -> value M.t
  val neg: vbool -> vbool M.t
  val neq: vint * vint -> unit M.t
  val one: vint
  val read: ident * state -> value M.t
  val toBoolean: value -> vbool M.t
  val toInteger: value -> vint M.t
  val true': vbool
  val write: ident * state * value -> state M.t
  val zero: vint
end

(** Module defining the specified terms *)
module MakeInterpreter (F: UNSPEC) = struct
  include F

  let ( let* ) = M.bind

  let apply1 = M.apply
  let apply2 f arg1 arg2 =
    let* _tmp = apply1 f arg1 in
    apply1 _tmp arg2

  let rec eqBoolean b1 =
    M.ret (function b2 ->
      M.branch [
        (function () ->
          let* _ = apply1 isTrue b1 in
          M.ret b2) ;
        (function () ->
          let* _ = apply1 isFalse b1 in
          apply1 neg b2)])
  and eval_expr s =
    M.ret (function e ->
      M.branch [
        (function () ->
          begin match e with
          | Const i -> apply1 litToVal i
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match e with
          | Var x -> apply1 read (x, s)
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match e with
          | Plus (t1, t2) ->
              let* v1 = apply2 eval_expr s t1 in
              let* v1 = apply1 toInteger v1 in
              let* v2 = apply2 eval_expr s t2 in
              let* v2 = apply1 toInteger v2 in
              let* v3 = apply1 add (v1, v2) in
              apply1 mkInt v3
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match e with
          | Equal (t1, t2) ->
              let* v1 = apply2 eval_expr s t1 in
              let* v2 = apply2 eval_expr s t2 in
              let* b =
                M.branch [
                  (function () ->
                    let* b1 = apply1 isBool v1 in
                    let* b2 = apply1 isBool v2 in
                    apply2 eqBoolean b1 b2) ;
                  (function () ->
                    let* (i1, i2) =
                      M.branch [
                        (function () ->
                          let* i1 = apply1 isInt v1 in
                          let* i2 = apply1 toInteger v2 in
                          M.ret (i1, i2)) ;
                        (function () ->
                          let* i1 = apply1 toInteger v1 in
                          let* i2 = apply1 isInt v2 in
                          M.ret (i1, i2))]
                    in
                    M.branch [
                      (function () ->
                        let* _ = apply1 eq (i1, i2) in
                        M.ret true') ;
                      (function () ->
                        let* _ = apply1 neq (i1, i2) in
                        M.ret false')])]
              in
              apply1 mkBool b
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match e with
          | Not t ->
              let* v = apply2 eval_expr s t in
              let* b = apply1 toBoolean v in
              let* n = apply1 neg b in
              apply1 mkBool n
          | _ -> M.fail ""
          end)])
  and eval_stmt s =
    M.ret (function t ->
      M.branch [
        (function () ->
          begin match t with
          | Skip -> M.ret s
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match t with
          | Assign (x, e) ->
              let* v = apply2 eval_expr s e in
              apply1 write (x, s, v)
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match t with
          | Seq (t1, t2) ->
              let* s = apply2 eval_stmt s t1 in
              apply2 eval_stmt s t2
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match t with
          | If (e, t1, t2) ->
              let* v = apply2 eval_expr s e in
              let* b = apply1 toBoolean v in
              M.branch [
                (function () ->
                  let* _ = apply1 isTrue b in
                  apply2 eval_stmt s t1) ;
                (function () ->
                  let* _ = apply1 isFalse b in
                  apply2 eval_stmt s t2)]
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match t with
          | While (e, t) ->
              let* v = apply2 eval_expr s e in
              let* b = apply1 toBoolean v in
              M.branch [
                (function () ->
                  let* _ = apply1 isTrue b in
                  let* s = apply2 eval_stmt s t in
                  apply2 eval_stmt s t) ;
                (function () ->
                  let* _ = apply1 isFalse b in
                  M.ret s)]
          | _ -> M.fail ""
          end)])
  and neg b =
    M.branch [
      (function () ->
        let* _ = apply1 isTrue b in
        M.ret false') ;
      (function () ->
        let* _ = apply1 isFalse b in
        M.ret true')]
  and toBoolean v =
    M.branch [
      (function () ->
        let* b = apply1 isBool v in
        M.ret b) ;
      (function () ->
        let* i = apply1 isInt v in
        let z = zero in
        M.branch [
          (function () ->
            let* _ = apply1 eq (i, z) in
            M.ret false') ;
          (function () ->
            let* _ = apply1 neq (i, z) in
            M.ret true')])]
  and toInteger v =
    M.branch [
      (function () ->
        let* i = apply1 isInt v in
        M.ret i) ;
      (function () ->
        let* b = apply1 isBool v in
        M.branch [
          (function () ->
            let* _ = apply1 isTrue b in
            M.ret one) ;
          (function () ->
            let* _ = apply1 isFalse b in
            M.ret zero)])]
end