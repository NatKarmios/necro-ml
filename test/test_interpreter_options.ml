(* option language test : needs an interpreter to be generated in "options.ml" *)

open Options

module T = struct
  type ident = string
  type lit = int
  type vint = int
  type vbool = bool
  type value = Int of vint | Bool of vbool
  type state = (ident, value) Hashtbl.t
end

module Input = struct
	include T
	include Unspec(Monads.ID)(T)
  let o_to_v = function
    | Option.Some s -> Some s
    | Option.None -> None
  let v_to_o = function
    | Some s -> Option.Some s
    | None -> Option.None

  let litToVal lit = Int lit
  let read (ident, state) = o_to_v (Hashtbl.find_opt state ident)
  let write (ident, state, value) =
    let () = Hashtbl.replace state ident value in
    state
  let isInt = function
    | Int i -> Some i
    | _ -> None
  let isBool = function
    | Bool b -> Some b
    | _ -> None
  let add (a, b) = Int (a + b)
  let eq (a, b) = Bool (a = b)
  let neg b = Bool (not b)
  let isTrue b = assert(b)
  let isFalse b = assert(not b)
end


module InterpWhile = MakeInterpreter (Input)

open Input
open InterpWhile

let initial_state () = Hashtbl.create 29

let get = function
  | Some x -> x
  | None -> assert false

let () =
  let t =
    Try (
      Assign("x", Var "y"),
      Assign("x", Const 0)
    )
  in
  let s = eval_stmt (initial_state(), t) in
  assert(Input.read ("x", get s) = Some (Int 0))

