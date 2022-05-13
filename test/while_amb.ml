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
  | Amb of expr alist
  and boolean =
  | True
  | False
  and 'a alist =
  | Nil
  | Cons of ('a * 'a alist)

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
  | Amb of expr alist
  and boolean =
  | True
  | False
  and 'a alist =
  | Nil
  | Cons of ('a * 'a alist)

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
  | Amb of expr alist
  and boolean =
  | True
  | False
  and 'a alist =
  | Nil
  | Cons of ('a * 'a alist)

  val add: vint * vint -> vint M.t
  val amb: 'a alist -> 'a M.t
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

  let rec amb: 'a. 'a alist -> 'a M.t  =
    function | (Cons (x, l)) ->
    M.branch [
      (function () ->
        M.ret x) ;
      (function () ->
        apply1 amb l)]
    
    | _ ->
    M.fail ""
    
  and eval_expr =
    function (s, e) ->
    begin match e with
    | Const i -> apply1 litToVal i
    | Var x -> apply1 read (x, s)
    | Plus (t1, t2) ->
        let* _tmp = apply1 eval_expr (s, t1) in
        begin match _tmp with
        | Int f1 ->
            let* _tmp = apply1 eval_expr (s, t2) in
            begin match _tmp with
            | Int f2 ->
                let* v = apply1 add (f1, f2) in
                M.ret (Int v)
            | _ -> M.fail ""
            end
        | _ -> M.fail ""
        end
    | Equal (t1, t2) ->
        let* _tmp = apply1 eval_expr (s, t1) in
        begin match _tmp with
        | Int f1 ->
            let* _tmp = apply1 eval_expr (s, t2) in
            begin match _tmp with
            | Int f2 ->
                let* v = apply1 eq (f1, f2) in
                M.ret (Bool v)
            | _ -> M.fail ""
            end
        | _ -> M.fail ""
        end
    | Amb l ->
        let* x = apply1 amb l in
        apply1 eval_expr (s, x)
    | Not t ->
        let* _tmp = apply1 eval_expr (s, t) in
        begin match _tmp with
        | Bool f1 ->
            let* b = apply1 neg f1 in
            M.ret (Bool b)
        | _ -> M.fail ""
        end
    end
  and eval_stmt =
    function (s, t) ->
    begin match t with
    | Skip -> M.ret s
    | Assign (t1, t2) ->
        let* f2 = apply1 eval_expr (s, t2) in
        apply1 write (t1, s, f2)
    | Seq (t1, t2) ->
        let* f1 = apply1 eval_stmt (s, t1) in
        apply1 eval_stmt (f1, t2)
    | If (t1, t2, t3) ->
        let* _tmp = apply1 eval_expr (s, t1) in
        begin match _tmp with
        | Bool f1 ->
            begin match f1 with
            | True -> apply1 eval_stmt (s, t2)
            | False -> apply1 eval_stmt (s, t3)
            end
        | _ -> M.fail ""
        end
    | While (t1, t2) ->
        let* _tmp = apply1 eval_expr (s, t1) in
        begin match _tmp with
        | Bool f1 ->
            begin match f1 with
            | True ->
                let* f2 = apply1 eval_stmt (s, t2) in
                apply1 eval_stmt (f2, While (t1, t2))
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