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

  type 'a vlist =
  | VNil
  | VCons of ('a * 'a vlist)
  and stmt =
  | While of (expr * stmt)
  | Skip
  | Seq of (stmt * stmt)
  | Parallel of (stmt * stmt)
  | If of (expr * stmt * stmt)
  | Assign of (ident * expr)
  and expr =
  | Var of ident
  | Plus of (expr * expr)
  | Not of expr
  | Equal of (expr * expr)
  | Const of lit

  val add: vint -> (vint -> value M.t) M.t
  val eq: vint -> (vint -> value M.t) M.t
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

  type 'a vlist =
  | VNil
  | VCons of ('a * 'a vlist)
  and stmt =
  | While of (expr * stmt)
  | Skip
  | Seq of (stmt * stmt)
  | Parallel of (stmt * stmt)
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

  type 'a vlist =
  | VNil
  | VCons of ('a * 'a vlist)
  and stmt =
  | While of (expr * stmt)
  | Skip
  | Seq of (stmt * stmt)
  | Parallel of (stmt * stmt)
  | If of (expr * stmt * stmt)
  | Assign of (ident * expr)
  and expr =
  | Var of ident
  | Plus of (expr * expr)
  | Not of expr
  | Equal of (expr * expr)
  | Const of lit

  val add: vint -> (vint -> value M.t) M.t
  val append: 'a vlist -> ('a vlist -> ('a vlist) M.t) M.t
  val eq: vint -> (vint -> value M.t) M.t
  val eval_expr: state -> (expr -> value M.t) M.t
  val eval_stmt: state -> (stmt -> state M.t) M.t
  val flatten: stmt -> (stmt vlist) M.t
  val isBool: value -> vbool M.t
  val isFalse: vbool -> unit M.t
  val isInt: value -> vint M.t
  val isTrue: vbool -> unit M.t
  val litToVal: lit -> value M.t
  val neg: vbool -> value M.t
  val read: ident * state -> value M.t
  val unflatten: stmt vlist -> stmt M.t
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

  let rec append: 'a. 'a vlist -> ('a vlist -> ('a vlist) M.t) M.t  =
    function l1 ->
    M.ret (function l2 ->
      begin match l1 with
      | VNil -> M.ret l2
      | VCons (x, q) ->
          let* l' = apply2 append q l2 in
          M.ret (VCons (x, l'))
      end)
  and eval_expr s =
    M.ret (function e ->
      begin match e with
      | Const i -> apply1 litToVal i
      | Var x -> apply1 read (x, s)
      | Plus (t1, t2) ->
          let* f1 = apply2 eval_expr s t1 in
          let* f1' = apply1 isInt f1 in
          let* f2 = apply2 eval_expr s t2 in
          let* f2' = apply1 isInt f2 in
          apply2 add f1' f2'
      | Equal (t1, t2) ->
          let* f1 = apply2 eval_expr s t1 in
          let* f1' = apply1 isInt f1 in
          let* f2 = apply2 eval_expr s t2 in
          let* f2' = apply1 isInt f2 in
          apply2 eq f1' f2'
      | Not t ->
          let* f1 = apply2 eval_expr s t in
          let* f1' = apply1 isBool f1 in
          apply1 neg f1'
      end)
  and eval_stmt s =
    M.ret (function t ->
      begin match t with
      | Skip -> M.ret s
      | Assign (t1, t2) ->
          let* f2 = apply2 eval_expr s t2 in
          apply1 write (t1, s, f2)
      | Seq (t1, t2) ->
          let* f1 = apply2 eval_stmt s t1 in
          apply2 eval_stmt f1 t2
      | If (t1, t2, t3) ->
          let* f1 = apply2 eval_expr s t1 in
          let* f1' = apply1 isBool f1 in
          M.branch [
            (function () ->
              let* _ = apply1 isTrue f1' in
              apply2 eval_stmt s t2) ;
            (function () ->
              let* _ = apply1 isFalse f1' in
              apply2 eval_stmt s t3)]
      | While (t1, t2) ->
          let* f1 = apply2 eval_expr s t1 in
          let* f1' = apply1 isBool f1 in
          M.branch [
            (function () ->
              let* _ = apply1 isTrue f1' in
              let* f2 = apply2 eval_stmt s t2 in
              apply2 eval_stmt f2 (While (t1, t2))) ;
            (function () ->
              let* _ = apply1 isFalse f1' in
              M.ret s)]
      | Parallel (Skip, Skip) -> M.ret s
      | Parallel (t1, t2) ->
          M.branch [
            (function () ->
              let* _tmp = apply1 flatten t1 in
              begin match _tmp with
              | VCons (x1, q1) ->
                  let* t'1 = apply1 unflatten q1 in
                  apply2 eval_stmt s (Seq (x1, Parallel (t'1, t2)))
              | _ -> M.fail ""
              end) ;
            (function () ->
              let* _tmp = apply1 flatten t2 in
              begin match _tmp with
              | VCons (x2, q2) ->
                  let* t'2 = apply1 unflatten q2 in
                  apply2 eval_stmt s (Seq (x2, Parallel (t1, t'2)))
              | _ -> M.fail ""
              end)]
      end)
  and flatten s =
    begin match s with
    | Skip -> M.ret VNil
    | Seq (s1, s2) ->
        let* l1 = apply1 flatten s1 in
        let* l2 = apply1 flatten s2 in
        apply2 append l1 l2
    | _ -> M.ret (VCons (s, VNil))
    end
  and unflatten x =
    begin match x with
    | VNil -> M.ret Skip
    | VCons (x, q) ->
        let* t2 = apply1 unflatten q in
        M.ret (Seq (x, t2))
    end
end