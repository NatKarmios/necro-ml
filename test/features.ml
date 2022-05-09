(** This file was automatically generated using necroml
	See https://gitlab.inria.fr/skeletons/necro-ml/ for more informations *)

(** The unspecified types *)
module type TYPES = sig
  type int
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

  type ('a, 'b) sum =
  | Right of 'b
  | Left of 'a
  and nat =
  | Zero
  | Succ of nat
  (** You can set special comments by beginning with two stars *)
  and 'a list =
  | Nil (** Depending on where they are placed, they are bound either to a type,
		a constructor, or a declaration *)
  | Cons of ('a * 'a list)
  and euler = {
  re: int ;
  im: int ;
  }
  and 'a option = (unit, 'a) sum

  val add: int -> (int -> int M.t) M.t
  val mul: int -> (int -> int M.t) M.t
  val one: int
  val opp: int -> int M.t
  val succ: int -> int M.t
  val zero: int
end

(** A default instantiation *)
module Unspec (M: MONAD) (T: TYPES) = struct
  exception NotImplemented of string
  include T
  module M = M

  type ('a, 'b) sum =
  | Right of 'b
  | Left of 'a
  and nat =
  | Zero
  | Succ of nat
  and 'a list =
  | Nil (** Depending on where they are placed, they are bound either to a type,
		a constructor, or a declaration *)
  | Cons of ('a * 'a list)
  and euler = {
  re: int ;
  im: int ;
  }
  and 'a option = (unit, 'a) sum

  let add _ = raise (NotImplemented "add")
  let mul _ = raise (NotImplemented "mul")
  let opp _ = raise (NotImplemented "opp")
  let succ _ = raise (NotImplemented "succ")
end

(** The module type for interpreters *)
module type INTERPRETER = sig
  module M: MONAD

  type int

  type ('a, 'b) sum =
  | Right of 'b
  | Left of 'a
  and nat =
  | Zero
  | Succ of nat
  (** You can set special comments by beginning with two stars *)
  and 'a list =
  | Nil (** Depending on where they are placed, they are bound either to a type,
		a constructor, or a declaration *)
  | Cons of ('a * 'a list)
  and euler = {
  re: int ;
  im: int ;
  }
  and 'a option = (unit, 'a) sum

  val add: int -> (int -> int M.t) M.t
  val add': nat -> (nat -> nat M.t) M.t
  val add'': euler * euler -> euler M.t
  val conj: euler -> euler M.t
  val get: 'a option -> 'a M.t
  val injl: 'a -> (('a, 'b) sum) M.t
  val injr: 'b -> (('a, 'b) sum) M.t
  val is_None: 'a option -> unit M.t
  val mul: int -> (int -> int M.t) M.t
  val none: 'a option
  val norm: euler -> int M.t
  val one: int
  val one': nat
  val opp: int -> int M.t
  val opt: 'a option -> (('a -> ('b option) M.t) -> ('b option) M.t) M.t
  val pred: nat -> (nat option) M.t
  val some: 'a -> ('a option) M.t
  val sub: nat -> (nat -> (nat option) M.t) M.t
  val succ: int -> int M.t
  val succ': nat -> nat M.t
  val zero: int
  val zero': nat
end

(** Module defining the specified terms *)
module MakeInterpreter (F: UNSPEC) = struct
  include F

  let ( let* ) = M.bind

  let apply1 = M.apply
  let apply2 f arg1 arg2 =
    let* _tmp = apply1 f arg1 in
    apply1 _tmp arg2

  let rec add' x =
    M.ret (function y ->
      begin match y with
      | Zero -> M.ret x
      | Succ y' ->
          let* n = apply2 add' x y' in
          M.ret (Succ n)
      end)
  and add'' e =
    let {re = re1; im = im1} = fst e in
    let {re = re2; im = im2} = snd e in
    let* re = apply2 add re1 re2 in
    let* im = apply2 add im1 im2 in
    M.ret {re = re; im = im}
  and conj e =
    let* im' = apply1 opp e.im in
    M.ret {e with im = im'}
  and get: 'a. 'a option -> 'a M.t  =
    function o ->
    begin match o with
    | Right a -> M.ret a
    | _ -> M.fail ""
    end
  and injl: 'a 'b. 'a -> (('a, 'b) sum) M.t  =
    function a ->
    M.ret (Left a)
  and injr: 'a 'b. 'b -> (('a, 'b) sum) M.t  =
    function b ->
    M.ret (Right b)
  and is_None: 'a. 'a option -> unit M.t  =
    function o ->
    begin match o with
    | Left () -> M.ret ()
    | _ -> M.fail ""
    end
  and none: 'a. 'a option  = Left ()
  and norm e =
    let* re2 = apply2 mul e.re e.re in
    let* im2 = apply2 mul e.im e.im in
    apply2 add re2 im2
  and one' = Succ zero'
  and opt: 'a 'b. 'a option -> (('a -> ('b option) M.t) -> ('b option) M.t) M.t  =
    function oa ->
    M.ret (function f ->
      M.branch [
        (function () ->
          let* () = apply1 is_None oa in
          M.ret none) ;
        (function () ->
          let* a = apply1 get oa in
          apply1 f a)])
  and pred n =
    M.branch [
      (function () ->
        begin match n with
        | Zero -> M.ret none
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match n with
        | Succ m -> apply1 some m
        | _ -> M.fail ""
        end)]
  and some: 'a. 'a -> ('a option) M.t  =
    function a ->
    M.ret (Right a)
  and sub m =
    M.ret (function n ->
      M.branch [
        (function () ->
          begin match n with
          | Zero -> apply1 some m
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match n with
          | Succ n' ->
              let* _tmp = apply1 pred m in
              apply2 opt _tmp (function m' ->
                apply2 sub m' n')
          | _ -> M.fail ""
          end)])
  and succ' x = M.ret (Succ x)
  and zero' = Zero
end