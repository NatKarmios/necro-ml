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
end

(** A default instantiation *)
module Unspec (M: MONAD) (T: TYPES) = struct
  exception NotImplemented of string
  include T
  module M = M

  
end

(** The module type for interpreters *)
module type INTERPRETER = sig
  module M: MONAD

  val dead: unit -> unit M.t
  val good: unit -> unit M.t
  val test: unit -> unit M.t
end

(** Module defining the specified terms *)
module MakeInterpreter (F: UNSPEC) = struct
  include F

  let ( let* ) = M.bind

  let apply1 = M.apply

  let rec dead =
    function _ ->
    M.branch []
  and good =
    function _ ->
    M.ret ()
  and test =
    function _ ->
    let* f =
      M.branch [
        (function () ->
          M.ret dead) ;
        (function () ->
          M.ret good)]
    in
    apply1 f ()
end