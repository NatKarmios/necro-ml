(** This file was automatically generated using necroml
	See https://gitlab.inria.fr/skeletons/necro-ml/ for more informations *)

(** The unspecified types *)
module type TYPES = sig
  type ident
  type lit
  type state
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

  type vbool =
  | True
  | False
  and value =
  | Int of vint
  | Bool of vbool
  and stmt =
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
  val eq: vint * vint -> vbool M.t
  val litToVal: lit -> value M.t
  val one: vint
  val read: ident * state -> value M.t
  val write: ident * state * value -> state M.t
  val zero: vint
end

(** A default instantiation *)
module Unspec (M: MONAD) (T: TYPES) = struct
  exception NotImplemented of string
  include T
  module M = M

  type vbool =
  | True
  | False
  and value =
  | Int of vint
  | Bool of vbool
  and stmt =
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
  let litToVal _ = raise (NotImplemented "litToVal")
  let read _ = raise (NotImplemented "read")
  let write _ = raise (NotImplemented "write")
end

(** The module type for interpreters *)
module type INTERPRETER = sig
  module M: MONAD

  type ident
  type lit
  type state
  type vint

  type vbool =
  | True
  | False
  and value =
  | Int of vint
  | Bool of vbool
  and stmt =
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
  val eq: vint * vint -> vbool M.t
  val eqBoolean: vbool -> (vbool -> vbool M.t) M.t
  val eval_expr: state -> (expr -> value M.t) M.t
  val eval_stmt: state -> (stmt -> state M.t) M.t
  val litToVal: lit -> value M.t
  val neg: vbool -> vbool M.t
  val one: vint
  val read: ident * state -> value M.t
  val toBoolean: value -> vbool M.t
  val toInteger: value -> vint M.t
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
      begin match b1 with
      | True -> M.ret b2
      | False -> apply1 neg b2
      end)
  and eval_expr s =
    M.ret (function e ->
      begin match e with
      | Const i -> apply1 litToVal i
      | Var x -> apply1 read (x, s)
      | Plus (t1, t2) ->
          let* v1 = apply2 eval_expr s t1 in
          let* v1 = apply1 toInteger v1 in
          let* v2 = apply2 eval_expr s t2 in
          let* v2 = apply1 toInteger v2 in
          let* v3 = apply1 add (v1, v2) in
          M.ret (Int v3)
      | Equal (t1, t2) ->
          let* v1 = apply2 eval_expr s t1 in
          let* v2 = apply2 eval_expr s t2 in
          let* b =
            begin match (v1, v2) with
            | (Bool b1, Bool b2) -> apply2 eqBoolean b1 b2
            | _ ->
                let* i1 = apply1 toInteger v1 in
                let* i2 = apply1 toInteger v2 in
                apply1 eq (i1, i2)
            end
          in
          M.ret (Bool b)
      | Not t ->
          let* v = apply2 eval_expr s t in
          let* b = apply1 toBoolean v in
          let* n = apply1 neg b in
          M.ret (Bool n)
      end)
  and eval_stmt s =
    M.ret (function t ->
      begin match t with
      | Skip -> M.ret s
      | Assign (x, e) ->
          let* v = apply2 eval_expr s e in
          apply1 write (x, s, v)
      | Seq (t1, t2) ->
          let* s = apply2 eval_stmt s t1 in
          apply2 eval_stmt s t2
      | If (e, t1, t2) ->
          let* v = apply2 eval_expr s e in
          let* b = apply1 toBoolean v in
          begin match b with
          | True -> apply2 eval_stmt s t1
          | False -> apply2 eval_stmt s t2
          end
      | While (e, t) ->
          let* v = apply2 eval_expr s e in
          let* b = apply1 toBoolean v in
          begin match b with
          | True ->
              let* s = apply2 eval_stmt s t in
              apply2 eval_stmt s t
          | False -> M.ret s
          end
      end)
  and neg b =
    begin match b with
    | True -> M.ret False
    | False -> M.ret True
    end
  and toBoolean v =
    begin match v with
    | Bool b -> M.ret b
    | Int i ->
        let z = zero in
        let* b = apply1 eq (i, z) in
        begin match b with
        | True -> M.ret False
        | False -> M.ret True
        end
    end
  and toInteger v =
    begin match v with
    | Int i -> M.ret i
    | Bool True -> M.ret one
    | Bool False -> M.ret zero
    end
end