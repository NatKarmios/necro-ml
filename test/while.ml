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

  type value =
  | Int of vint
  | Bool of boolean
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
  and boolean =
  | True
  | False
  and 'a st = state -> ('a * state) M.t

  val add: vint * vint -> vint M.t
  val eq: vint * vint -> boolean M.t
  val litToVal: lit -> value M.t
  val read: ident -> (value st) M.t
  val write: ident * value -> (unit st) M.t
end

(** A default instantiation *)
module Unspec (M: MONAD) (T: TYPES) = struct
  exception NotImplemented of string
  include T
  module M = M

  type value =
  | Int of vint
  | Bool of boolean
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
  and boolean =
  | True
  | False
  and 'a st = state -> ('a * state) M.t

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

  type value =
  | Int of vint
  | Bool of boolean
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
  and boolean =
  | True
  | False
  and 'a st = state -> ('a * state) M.t

  val add: vint * vint -> vint M.t
  val bind: 'a st -> (('a -> ('b st) M.t) -> ('b st) M.t) M.t
  val eq: vint * vint -> boolean M.t
  val eval_expr: expr -> (value st) M.t
  val eval_stmt: stmt -> (unit st) M.t
  val litToVal: lit -> value M.t
  val neg: boolean -> boolean M.t
  val read: ident -> (value st) M.t
  val ret: 'a -> ('a st) M.t
  val write: ident * value -> (unit st) M.t
end

(** Module defining the specified terms *)
module MakeInterpreter (F: UNSPEC) = struct
  include F

  let ( let* ) = M.bind

  let apply1 = M.apply
  let apply2 f arg1 arg2 =
    let* _tmp = apply1 f arg1 in
    apply1 _tmp arg2

  let rec bind: 'a 'b. 'a st -> (('a -> ('b st) M.t) -> ('b st) M.t) M.t  =
    function a ->
    M.ret (function f ->
      M.ret (function s ->
        let* (a, s) = apply1 a s in
        apply2 f a s))
  and eval_expr e =
    begin match e with
    | Const i ->
        let* v = apply1 litToVal i in
        apply1 ret v
    | Var x -> apply1 read x
    | Plus (t1, t2) ->
        let* _tmp = apply1 eval_expr t1 in
        apply2 bind _tmp (function f1 ->
          begin match f1 with
          | Int f1' ->
              let* _tmp = apply1 eval_expr t2 in
              apply2 bind _tmp (function f2 ->
                begin match f2 with
                | Int f2' ->
                    let* v = apply1 add (f1', f2') in
                    let r = Int v in
                    apply1 ret r
                | _ -> M.fail ""
                end)
          | _ -> M.fail ""
          end)
    | Equal (t1, t2) ->
        let* _tmp = apply1 eval_expr t1 in
        apply2 bind _tmp (function f1 ->
          begin match f1 with
          | Int f1' ->
              let* _tmp = apply1 eval_expr t2 in
              apply2 bind _tmp (function f2 ->
                begin match f2 with
                | Int f2' ->
                    let* v = apply1 eq (f1', f2') in
                    let r = Bool v in
                    apply1 ret r
                | _ -> M.fail ""
                end)
          | _ -> M.fail ""
          end)
    | Not t ->
        let* _tmp = apply1 eval_expr t in
        apply2 bind _tmp (function f1 ->
          begin match f1 with
          | Bool f1' ->
              let* v = apply1 neg f1' in
              let r = Bool v in
              apply1 ret r
          | _ -> M.fail ""
          end)
    end
  and eval_stmt t =
    begin match t with
    | Skip -> apply1 ret ()
    | Assign (t1, t2) ->
        let* _tmp = apply1 eval_expr t2 in
        apply2 bind _tmp (function f2 ->
          apply1 write (t1, f2))
    | Seq (t1, t2) ->
        let* _tmp = apply1 eval_stmt t1 in
        apply2 bind _tmp (function _ ->
          apply1 eval_stmt t2)
    | If (t1, t2, t3) ->
        let* _tmp = apply1 eval_expr t1 in
        apply2 bind _tmp (function f1 ->
          begin match f1 with
          | Bool f1' ->
              begin match f1' with
              | True -> apply1 eval_stmt t2
              | False -> apply1 eval_stmt t3
              end
          | _ -> M.fail ""
          end)
    | While (t1, t2) ->
        let* _tmp = apply1 eval_expr t1 in
        apply2 bind _tmp (function f1 ->
          begin match f1 with
          | Bool f1' ->
              begin match f1' with
              | True ->
                  let* _tmp = apply1 eval_stmt t2 in
                  apply2 bind _tmp (function _ ->
                    apply1 eval_stmt (While (t1, t2)))
              | False -> apply1 ret ()
              end
          | _ -> M.fail ""
          end)
    end
  and neg b =
    begin match b with
    | True -> M.ret False
    | False -> M.ret True
    end
  and ret: 'a. 'a -> ('a st) M.t  =
    function a ->
    M.ret (function s ->
      M.ret (a, s))
end