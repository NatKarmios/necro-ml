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

  type 'a voption =
  | Some of 'a
  | None
  and stmt =
  | While of (expr * stmt)
  | Try of (stmt * stmt)
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

  val add: vint * vint -> value M.t
  val eq: vint * vint -> value M.t
  val isBool: value -> (vbool voption) M.t
  val isFalse: vbool -> unit M.t
  val isInt: value -> (vint voption) M.t
  val isTrue: vbool -> unit M.t
  val litToVal: lit -> value M.t
  val neg: vbool -> value M.t
  val read: ident * state -> (value voption) M.t
  val write: ident * state * value -> state M.t
end

(** A default instantiation *)
module Unspec (M: MONAD) (T: TYPES) = struct
  exception NotImplemented of string
  include T
  module M = M

  type 'a voption =
  | Some of 'a
  | None
  and stmt =
  | While of (expr * stmt)
  | Try of (stmt * stmt)
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

  type 'a voption =
  | Some of 'a
  | None
  and stmt =
  | While of (expr * stmt)
  | Try of (stmt * stmt)
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

  val add: vint * vint -> value M.t
  val eq: vint * vint -> value M.t
  val eval_expr: state * expr -> (value voption) M.t
  val eval_stmt: state * stmt -> (state voption) M.t
  val isBool: value -> (vbool voption) M.t
  val isFalse: vbool -> unit M.t
  val isInt: value -> (vint voption) M.t
  val isTrue: vbool -> unit M.t
  val litToVal: lit -> value M.t
  val neg: vbool -> value M.t
  val opt: 'a voption -> (('a -> ('b voption) M.t) -> ('b voption) M.t) M.t
  val read: ident * state -> (value voption) M.t
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

  let rec eval_expr =
    function (s, e) ->
    begin match e with
    | Const i ->
        let* v = apply1 litToVal i in
        M.ret (Some v)
    | Var x -> apply1 read (x, s)
    | Plus (t1, t2) ->
        let* _tmp = apply1 eval_expr (s, t1) in
        apply2 opt _tmp (function f1 ->
          let* _tmp = apply1 eval_expr (s, t2) in
          apply2 opt _tmp (function f2 ->
            let* _tmp = apply1 isInt f1 in
            apply2 opt _tmp (function v1 ->
              let* _tmp = apply1 isInt f2 in
              apply2 opt _tmp (function v2 ->
                let* v = apply1 add (v1, v2) in
                M.ret (Some v)))))
    | Equal (t1, t2) ->
        let* _tmp = apply1 eval_expr (s, t1) in
        apply2 opt _tmp (function f1 ->
          let* _tmp = apply1 eval_expr (s, t2) in
          apply2 opt _tmp (function f2 ->
            let* _tmp = apply1 isInt f1 in
            apply2 opt _tmp (function v1 ->
              let* _tmp = apply1 isInt f2 in
              apply2 opt _tmp (function v2 ->
                let* v = apply1 eq (v1, v2) in
                M.ret (Some v)))))
    | Not e ->
        let* _tmp = apply1 eval_expr (s, e) in
        apply2 opt _tmp (function f ->
          let* _tmp = apply1 isBool f in
          apply2 opt _tmp (function v ->
            let* v' = apply1 neg v in
            M.ret (Some v')))
    end
  and eval_stmt =
    function (s, t) ->
    begin match t with
    | Skip -> M.ret (Some s)
    | Seq (s1, s2) ->
        let* _tmp = apply1 eval_stmt (s, s1) in
        apply2 opt _tmp (function f1 ->
          apply1 eval_stmt (f1, s2))
    | Assign (t1, t2) ->
        let* _tmp = apply1 eval_expr (s, t2) in
        apply2 opt _tmp (function f2 ->
          let* t' = apply1 write (t1, s, f2) in
          M.ret (Some t'))
    | If (t1, t2, t3) ->
        let* _tmp = apply1 eval_expr (s, t1) in
        apply2 opt _tmp (function f1 ->
          let* _tmp = apply1 isBool f1 in
          apply2 opt _tmp (function f1' ->
            M.branch [
              (function () ->
                let* () = apply1 isTrue f1' in
                apply1 eval_stmt (s, t2)) ;
              (function () ->
                let* () = apply1 isFalse f1' in
                apply1 eval_stmt (s, t3))]))
    | While (t1, t2) ->
        let* _tmp = apply1 eval_expr (s, t1) in
        apply2 opt _tmp (function f1 ->
          let* _tmp = apply1 isBool f1 in
          apply2 opt _tmp (function b ->
            M.branch [
              (function () ->
                let* () = apply1 isTrue b in
                let* _tmp = apply1 eval_stmt (s, t2) in
                apply2 opt _tmp (function s' ->
                  apply1 eval_stmt (s', While (t1, t2)))) ;
              (function () ->
                let* () = apply1 isFalse b in
                M.ret (Some s))]))
    | Try (t1, t2) ->
        let* s' = apply1 eval_stmt (s, t1) in
        begin match s' with
        | Some _ -> M.ret s'
        | None -> apply1 eval_stmt (s, t2)
        end
    end
  and opt: 'a 'b. 'a voption -> (('a -> ('b voption) M.t) -> ('b voption) M.t) M.t  =
    function o ->
    M.ret (function f ->
      begin match o with
      | None -> M.ret None
      | Some a -> apply1 f a
      end)
end