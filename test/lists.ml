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

  type 'a option =
  | Some of 'a
  | None
  and 'a list =
  | Nil
  | Cons of ('a * 'a list)
  and boolean =
  | True
  | False
end

(** A default instantiation *)
module Unspec (M: MONAD) (T: TYPES) = struct
  exception NotImplemented of string
  include T
  module M = M

  type 'a option =
  | Some of 'a
  | None
  and 'a list =
  | Nil
  | Cons of ('a * 'a list)
  and boolean =
  | True
  | False
end

(** The module type for interpreters *)
module type INTERPRETER = sig
  module M: MONAD

  type 'a option =
  | Some of 'a
  | None
  and 'a list =
  | Nil
  | Cons of ('a * 'a list)
  and boolean =
  | True
  | False

  val andb: boolean -> (boolean -> boolean M.t) M.t
  val append: 'a list -> ('a list -> ('a list) M.t) M.t
  val combine: 'a list -> ('b list -> (('a * 'b) list) M.t) M.t
  val exists: ('a -> boolean M.t) -> ('a list -> boolean M.t) M.t
  val filter: ('a -> boolean M.t) -> ('a list -> ('a list) M.t) M.t
  val find: ('a -> boolean M.t) -> ('a list -> 'a M.t) M.t
  val find_first: ('a -> boolean M.t) -> ('a list -> 'a M.t) M.t
  val find_first_opt: ('a -> boolean M.t) -> ('a list -> ('a option) M.t) M.t
  val find_opt: ('a -> boolean M.t) -> ('a list -> ('a option) M.t) M.t
  val flatten: ('a list) list -> ('a list) M.t
  val fold_left: ('a -> ('b -> 'a M.t) M.t) -> ('a -> ('b list -> 'a M.t) M.t) M.t
  val for_all: ('a -> boolean M.t) -> ('a list -> boolean M.t) M.t
  val insert: ('a -> ('a -> boolean M.t) M.t) -> ('a -> ('a list -> ('a list) M.t) M.t) M.t
  val iter: ('a -> unit M.t) -> ('a list -> unit M.t) M.t
  val map: ('a -> 'b M.t) -> ('a list -> ('b list) M.t) M.t
  val orb: boolean -> (boolean -> boolean M.t) M.t
  val sort: ('a -> ('a -> boolean M.t) M.t) -> ('a list -> ('a list) M.t) M.t
  val split: ('a * 'b) list -> ('a list * 'b list) M.t
  val unfold: ('a option) list -> (('a list) option) M.t
end

(** Module defining the specified terms *)
module MakeInterpreter (F: UNSPEC) = struct
  include F

  let ( let* ) = M.bind

  let apply1 = M.apply
  let apply2 f arg1 arg2 =
    let* _tmp = apply1 f arg1 in
    apply1 _tmp arg2
  let apply3 f arg1 arg2 arg3 =
    let* _tmp = apply1 f arg1 in
    apply2 _tmp arg2 arg3

  let rec andb b1 =
    M.ret (function b2 ->
      M.branch [
        (function () ->
          begin match b1 with
          | False -> M.ret False
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match b1 with
          | True -> M.ret b2
          | _ -> M.fail ""
          end)])
  and append: 'a. 'a list -> ('a list -> ('a list) M.t) M.t  =
    function l1 ->
    M.ret (function l2 ->
      M.branch [
        (function () ->
          begin match l1 with
          | Nil -> M.ret l2
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match l1 with
          | Cons (x, q) ->
              let* aux = apply2 append q l2 in
              M.ret (Cons (x, aux))
          | _ -> M.fail ""
          end)])
  and combine: 'a 'b. 'a list -> ('b list -> (('a * 'b) list) M.t) M.t  =
    function la ->
    M.ret (function lb ->
      M.branch [
        (function () ->
          begin match (la, lb) with
          | (Nil, Nil) -> M.ret Nil
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match (la, lb) with
          | (Cons (xa, qa), Cons (xb, qb)) ->
              let* aux = apply2 combine qa qb in
              M.ret (Cons ((xa, xb), aux))
          | _ -> M.fail ""
          end)])
  and exists: 'a. ('a -> boolean M.t) -> ('a list -> boolean M.t) M.t  =
    function f ->
    M.ret (function l ->
      M.branch [
        (function () ->
          begin match l with
          | Nil -> M.ret False
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match l with
          | Cons (x, q) ->
              let* b = apply1 f x in
              let* b' = apply2 exists f q in
              apply2 orb b b'
          | _ -> M.fail ""
          end)])
  and filter: 'a. ('a -> boolean M.t) -> ('a list -> ('a list) M.t) M.t  =
    function f ->
    M.ret (function l ->
      M.branch [
        (function () ->
          begin match l with
          | Nil -> M.ret Nil
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match l with
          | Cons (x, q) ->
              let* _tmp = apply1 f x in
              begin match _tmp with
              | False -> apply2 filter f q
              | _ -> M.fail ""
              end
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match l with
          | Cons (x, q) ->
              let* _tmp = apply1 f x in
              begin match _tmp with
              | True ->
                  let* q' = apply2 filter f q in
                  M.ret (Cons (x, q'))
              | _ -> M.fail ""
              end
          | _ -> M.fail ""
          end)])
  and find: 'a. ('a -> boolean M.t) -> ('a list -> 'a M.t) M.t  =
    function f ->
    M.ret (function l ->
      let* _tmp = apply2 find_opt f l in
      begin match _tmp with
      | Some x -> M.ret x
      | _ -> M.fail ""
      end)
  and find_first: 'a. ('a -> boolean M.t) -> ('a list -> 'a M.t) M.t  =
    function f ->
    M.ret (function l ->
      let* _tmp = apply2 find_first_opt f l in
      begin match _tmp with
      | Some x -> M.ret x
      | _ -> M.fail ""
      end)
  and find_first_opt: 'a. ('a -> boolean M.t) -> ('a list -> ('a option) M.t) M.t  =
    function f ->
    M.ret (function l ->
      M.branch [
        (function () ->
          begin match l with
          | Nil -> M.ret None
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match l with
          | Cons (x, _) ->
              let* _tmp = apply1 f x in
              begin match _tmp with
              | True -> M.ret (Some x)
              | _ -> M.fail ""
              end
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match l with
          | Cons (x, q) ->
              let* _tmp = apply1 f x in
              begin match _tmp with
              | False -> apply2 find_first_opt f q
              | _ -> M.fail ""
              end
          | _ -> M.fail ""
          end)])
  and find_opt: 'a. ('a -> boolean M.t) -> ('a list -> ('a option) M.t) M.t  =
    function f ->
    M.ret (function l ->
      M.branch [
        (function () ->
          begin match l with
          | Nil -> M.ret None
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match l with
          | Cons (x, _) ->
              let* _tmp = apply1 f x in
              begin match _tmp with
              | True -> M.ret (Some x)
              | _ -> M.fail ""
              end
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match l with
          | Cons (_, q) ->
              let* _tmp = apply2 find_opt f q in
              begin match _tmp with
              | Some y -> M.ret (Some y)
              | _ -> M.fail ""
              end
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match l with
          | Cons (x, q) ->
              let* _tmp = apply2 find_opt f q in
              begin match _tmp with
              | None ->
                  let* _tmp = apply1 f x in
                  begin match _tmp with
                  | False -> M.ret None
                  | _ -> M.fail ""
                  end
              | _ -> M.fail ""
              end
          | _ -> M.fail ""
          end)])
  and flatten: 'a. ('a list) list -> ('a list) M.t  =
    function l ->
    M.branch [
      (function () ->
        begin match l with
        | Nil -> M.ret Nil
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match l with
        | Cons (x, q) ->
            let* aux = apply1 flatten q in
            apply2 append x aux
        | _ -> M.fail ""
        end)]
  and fold_left: 'a 'b. ('a -> ('b -> 'a M.t) M.t) -> ('a -> ('b list -> 'a M.t) M.t) M.t  =
    function f ->
    M.ret (function a ->
      M.ret (function l ->
        M.branch [
          (function () ->
            begin match l with
            | Nil -> M.ret a
            | _ -> M.fail ""
            end) ;
          (function () ->
            begin match l with
            | Cons (x, q) ->
                let* a' = apply2 f a x in
                apply3 fold_left f a' q
            | _ -> M.fail ""
            end)]))
  and for_all: 'a. ('a -> boolean M.t) -> ('a list -> boolean M.t) M.t  =
    function f ->
    M.ret (function l ->
      M.branch [
        (function () ->
          begin match l with
          | Nil -> M.ret True
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match l with
          | Cons (x, q) ->
              let* b = apply1 f x in
              let* b' = apply2 for_all f q in
              apply2 andb b b'
          | _ -> M.fail ""
          end)])
  and insert: 'a. ('a -> ('a -> boolean M.t) M.t) -> ('a -> ('a list -> ('a list) M.t) M.t) M.t  =
    function le ->
    M.ret (function x ->
      M.ret (function l ->
        M.branch [
          (function () ->
            begin match l with
            | Nil -> M.ret (Cons (x, Nil))
            | _ -> M.fail ""
            end) ;
          (function () ->
            begin match l with
            | Cons (y, q) ->
                let* cmp = apply2 le x y in
                M.branch [
                  (function () ->
                    begin match cmp with
                    | True -> M.ret (Cons (x, Cons (y, q)))
                    | _ -> M.fail ""
                    end) ;
                  (function () ->
                    begin match cmp with
                    | False ->
                        let* aux = apply3 insert le x q in
                        M.ret (Cons (y, aux))
                    | _ -> M.fail ""
                    end)]
            | _ -> M.fail ""
            end)]))
  and iter: 'a. ('a -> unit M.t) -> ('a list -> unit M.t) M.t  =
    function f ->
    M.ret (function l ->
      M.branch [
        (function () ->
          begin match l with
          | Nil -> M.ret ()
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match l with
          | Cons (x, q) ->
              let* () = apply1 f x in
              apply2 iter f q
          | _ -> M.fail ""
          end)])
  and map: 'a 'b. ('a -> 'b M.t) -> ('a list -> ('b list) M.t) M.t  =
    function f ->
    M.ret (function l ->
      M.branch [
        (function () ->
          begin match l with
          | Nil -> M.ret Nil
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match l with
          | Cons (x, q) ->
              let* x' = apply1 f x in
              let* q' = apply2 map f q in
              M.ret (Cons (x', q'))
          | _ -> M.fail ""
          end)])
  and orb b1 =
    M.ret (function b2 ->
      M.branch [
        (function () ->
          begin match b1 with
          | True -> M.ret True
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match b1 with
          | False -> M.ret b2
          | _ -> M.fail ""
          end)])
  and sort: 'a. ('a -> ('a -> boolean M.t) M.t) -> ('a list -> ('a list) M.t) M.t  =
    function le ->
    M.ret (function l ->
      M.branch [
        (function () ->
          begin match l with
          | Nil -> M.ret l
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match l with
          | Cons (x, q) ->
              let* aux = apply2 sort le q in
              apply3 insert le x aux
          | _ -> M.fail ""
          end)])
  and split: 'a 'b. ('a * 'b) list -> ('a list * 'b list) M.t  =
    function l ->
    M.branch [
      (function () ->
        begin match l with
        | Nil -> M.ret (Nil, Nil)
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match l with
        | Cons ((x1, x2), q) ->
            let* (aux1, aux2) = apply1 split q in
            M.ret (Cons (x1, aux1), Cons (x2, aux2))
        | _ -> M.fail ""
        end)]
  and unfold: 'a. ('a option) list -> (('a list) option) M.t  =
    function l ->
    M.branch [
      (function () ->
        begin match l with
        | Nil -> M.ret (Some Nil)
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match l with
        | Cons (x, q) ->
            let* q = apply1 unfold q in
            M.branch [
              (function () ->
                begin match x with
                | None -> M.ret None
                | _ -> M.fail ""
                end) ;
              (function () ->
                begin match q with
                | None -> M.ret None
                | _ -> M.fail ""
                end) ;
              (function () ->
                begin match x with
                | Some x ->
                    begin match q with
                    | Some q ->
                        let l' = Cons (x, q) in
                        M.ret (Some l')
                    | _ -> M.fail ""
                    end
                | _ -> M.fail ""
                end)]
        | _ -> M.fail ""
        end)]
end