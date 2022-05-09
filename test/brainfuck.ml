(** This file was automatically generated using necroml
	See https://gitlab.inria.fr/skeletons/necro-ml/ for more informations *)

(** The unspecified types *)
module type TYPES = sig
  
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
  | Nil
  | Cons of ('a * 'a vlist)
  and state =
  | State of (ascii vlist * ascii * ascii vlist)
  and op =
  | Plus
  | Out
  | Minus
  | Loop of op vlist
  | Less
  | In
  | Greater
  and bit =
  | Zero
  | One
  and ascii =
  | Ascii of (bit * bit * bit * bit * bit * bit * bit)

  val input: unit -> ascii M.t
  val output: ascii -> unit M.t
end

(** A default instantiation *)
module Unspec (M: MONAD) (T: TYPES) = struct
  exception NotImplemented of string
  include T
  module M = M

  type 'a vlist =
  | Nil
  | Cons of ('a * 'a vlist)
  and state =
  | State of (ascii vlist * ascii * ascii vlist)
  and op =
  | Plus
  | Out
  | Minus
  | Loop of op vlist
  | Less
  | In
  | Greater
  and bit =
  | Zero
  | One
  and ascii =
  | Ascii of (bit * bit * bit * bit * bit * bit * bit)

  let input _ = raise (NotImplemented "input")
  let output _ = raise (NotImplemented "output")
end

(** The module type for interpreters *)
module type INTERPRETER = sig
  module M: MONAD

  type 'a vlist =
  | Nil
  | Cons of ('a * 'a vlist)
  and state =
  | State of (ascii vlist * ascii * ascii vlist)
  and op =
  | Plus
  | Out
  | Minus
  | Loop of op vlist
  | Less
  | In
  | Greater
  and bit =
  | Zero
  | One
  and ascii =
  | Ascii of (bit * bit * bit * bit * bit * bit * bit)

  (** Return carry and result *)
  val addbit: bit -> (bit -> (bit * bit) M.t) M.t
  val bind: (state -> ('a * state) M.t) -> (('a -> (state -> ('b * state) M.t) M.t) -> (state -> ('b * state) M.t) M.t) M.t
  val decr: state -> (unit * state) M.t
  val decr_ascii: ascii -> ascii M.t
  val eval: op vlist -> (state -> (unit * state) M.t) M.t
  val get: state -> (ascii * state) M.t
  val go_left: state -> (unit * state) M.t
  val go_right: state -> (unit * state) M.t
  val incr: state -> (unit * state) M.t
  val incr_ascii: ascii -> ascii M.t
  val input: unit -> ascii M.t
  val isZero: ascii -> bit M.t
  val output: ascii -> unit M.t
  val ret: 'a -> (state -> ('a * state) M.t) M.t
  val set: ascii -> (state -> (unit * state) M.t) M.t
  (** Return carry and result *)
  val subbit: bit -> (bit -> (bit * bit) M.t) M.t
  val zero: ascii
end

(** Module defining the specified terms *)
module MakeInterpreter (F: UNSPEC) = struct
  include F

  let ( let* ) = M.bind

  let apply1 = M.apply
  let apply2 f arg1 arg2 =
    let* _tmp = apply1 f arg1 in
    apply1 _tmp arg2

  (** Return carry and result *)
  let rec addbit b1 =
    M.ret (function b2 ->
      begin match (b1, b2) with
      | (Zero, _) -> M.ret (Zero, b2)
      | (One, Zero) -> M.ret (Zero, One)
      | (One, One) -> M.ret (One, Zero)
      end)
  and bind: 'a 'b. (state -> ('a * state) M.t) -> (('a -> (state -> ('b * state) M.t) M.t) -> (state -> ('b * state) M.t) M.t) M.t  =
    function a ->
    M.ret (function f ->
      M.ret (function s ->
        let* (a', s') = apply1 a s in
        apply2 f a' s'))
  and decr =
    function (State (l1, b, l2)) ->
    let* b' = apply1 decr_ascii b in
    M.ret ((), State (l1, b', l2))
  and decr_ascii =
    function (Ascii (b1, b2, b3, b4, b5, b6, b7)) ->
    let c = One in
    let* (c, b7) = apply2 subbit b7 c in
    let* (c, b6) = apply2 subbit b6 c in
    let* (c, b5) = apply2 subbit b5 c in
    let* (c, b4) = apply2 subbit b4 c in
    let* (c, b3) = apply2 subbit b3 c in
    let* (c, b2) = apply2 subbit b2 c in
    let* (_, b1) = apply2 subbit b1 c in
    M.ret (Ascii (b1, b2, b3, b4, b5, b6, b7))
  and eval o =
    begin match o with
    | Nil -> apply1 ret ()
    | Cons (op, l) ->
        begin match op with
        | Minus ->
            apply2 bind decr (function _ ->
              apply1 eval l)
        | Plus ->
            apply2 bind incr (function _ ->
              apply1 eval l)
        | Less ->
            apply2 bind go_left (function _ ->
              apply1 eval l)
        | Greater ->
            apply2 bind go_right (function _ ->
              apply1 eval l)
        | In ->
            let* v = apply1 input () in
            let* _tmp = apply1 set v in
            apply2 bind _tmp (function _ ->
              apply1 eval l)
        | Out ->
            apply2 bind get (function v ->
              let* () = apply1 output v in
              apply1 eval l)
        | Loop loop ->
            apply2 bind get (function v ->
              let* z = apply1 isZero v in
              begin match z with
              | One -> apply1 eval l
              | Zero ->
                  let* _tmp = apply1 eval loop in
                  apply2 bind _tmp (function _ ->
                    apply1 eval o)
              end)
        end
    end
  and get =
    function (State (l1, b, l2)) ->
    M.ret (b, State (l1, b, l2))
  and go_left =
    function (State (l1, b, l2)) ->
    let* (b', l1') =
      begin match l1 with
      | Nil -> M.ret (zero, Nil)
      | Cons x -> M.ret x
      end
    in
    M.ret ((), State (l1', b', Cons (b, l2)))
  and go_right =
    function (State (l1, b, l2)) ->
    let* (b', l2') =
      begin match l2 with
      | Nil -> M.ret (zero, Nil)
      | Cons x -> M.ret x
      end
    in
    M.ret ((), State (Cons (b, l1), b', l2'))
  and incr =
    function (State (l1, b, l2)) ->
    let* b' = apply1 incr_ascii b in
    M.ret ((), State (l1, b', l2))
  and incr_ascii a =
    let Ascii (b1, b2, b3, b4, b5, b6, b7) = a in
    let c = One in
    let* (c, b7) = apply2 addbit c b7 in
    let* (c, b6) = apply2 addbit c b6 in
    let* (c, b5) = apply2 addbit c b5 in
    let* (c, b4) = apply2 addbit c b4 in
    let* (c, b3) = apply2 addbit c b3 in
    let* (c, b2) = apply2 addbit c b2 in
    let* (_, b1) = apply2 addbit c b1 in
    M.ret (Ascii (b1, b2, b3, b4, b5, b6, b7))
  and isZero a =
    begin match a with
    | Ascii (Zero, Zero, Zero, Zero, Zero, Zero, Zero) -> M.ret One
    | _ -> M.ret Zero
    end
  and ret: 'a. 'a -> (state -> ('a * state) M.t) M.t  =
    function x ->
    M.ret (function s ->
      M.ret (x, s))
  and set b =
    M.ret (function (State (l1, _, l2)) ->
      M.ret ((), State (l1, b, l2)))
  (** Return carry and result *)
  and subbit b1 =
    M.ret (function b2 ->
      begin match (b1, b2) with
      | (b, Zero) -> M.ret (Zero, b)
      | (One, One) -> M.ret (Zero, Zero)
      | (Zero, One) -> M.ret (One, One)
      end)
  and zero = Ascii (Zero, Zero, Zero, Zero, Zero, Zero, Zero)
end