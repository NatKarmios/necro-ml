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

  val add: vint * vint -> vint M.t
  val eq: vint * vint -> boolean M.t
  val litToVal: lit -> value M.t
  val read: ident * state -> value M.t
  val write: ident * state * value -> state M.t
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

  val add: vint * vint -> vint M.t
  val eq: vint * vint -> boolean M.t
  val eval_expr: state * expr -> value M.t
  val eval_stmt: state * stmt -> state M.t
  val litToVal: lit -> value M.t
  val neg: boolean -> boolean M.t
  val read: ident * state -> value M.t
  val write: ident * state * value -> state M.t
end

(** Module defining the specified terms *)
module MakeInterpreter (F: UNSPEC) = struct
  include F

  let ( let* ) = M.bind

  let apply1 = M.apply

  let rec eval_expr =
    function (s, e) ->
    begin match e with
    | Const i -> apply1 litToVal i
    | Var x -> apply1 read (x, s)
    | Plus (e1, e2) ->
        let* _tmp = apply1 eval_expr (s, e1) in
        begin match _tmp with
        | Int i1 ->
            let* _tmp = apply1 eval_expr (s, e2) in
            begin match _tmp with
            | Int i2 ->
                let* i = apply1 add (i1, i2) in
                M.ret (Int i)
            | _ -> M.fail ""
            end
        | _ -> M.fail ""
        end
    | Equal (e1, e2) ->
        let* _tmp = apply1 eval_expr (s, e1) in
        begin match _tmp with
        | Int i1 ->
            let* _tmp = apply1 eval_expr (s, e2) in
            begin match _tmp with
            | Int i2 ->
                let* b = apply1 eq (i1, i2) in
                M.ret (Bool b)
            | _ -> M.fail ""
            end
        | _ -> M.fail ""
        end
    | Not e ->
        let* _tmp = apply1 eval_expr (s, e) in
        begin match _tmp with
        | Bool b ->
            let* b' = apply1 neg b in
            M.ret (Bool b')
        | _ -> M.fail ""
        end
    end
  and eval_stmt =
    function (s, t) ->
    begin match t with
    | Skip -> M.ret s
    | Assign (x, e) ->
        let* v = apply1 eval_expr (s, e) in
        apply1 write (x, s, v)
    | Seq (t1, t2) ->
        let* s' = apply1 eval_stmt (s, t1) in
        apply1 eval_stmt (s', t2)
    | If (cond, true', false') ->
        let* _tmp = apply1 eval_expr (s, cond) in
        begin match _tmp with
        | Bool b ->
            begin match b with
            | True -> apply1 eval_stmt (s, true')
            | False -> apply1 eval_stmt (s, false')
            end
        | _ -> M.fail ""
        end
    | While (cond, t') ->
        let* _tmp = apply1 eval_expr (s, cond) in
        begin match _tmp with
        | Bool b ->
            begin match b with
            | True ->
                let* s' = apply1 eval_stmt (s, t') in
                apply1 eval_stmt (s', t)
            | False -> M.ret s
            end
        | _ -> M.fail ""
        end
    end
  and neg b =
    begin match b with
    | True -> M.ret False
    | False -> M.ret True
    end
end