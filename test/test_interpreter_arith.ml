open Arith

module Types = struct
  type literal = int
  type value = int
end

module Input(M:MONAD) = struct
	include Unspec(M)(Types)
  let litToVal l = M.ret l
  let add l1 = M.ret (fun l2 -> M.ret (l1 + l2))
  let sub l1 = M.ret (fun l2 -> M.ret (l1 - l2))
  let mult l1 = M.ret (fun l2 -> M.ret (l1 * l2))
  let div l1 = M.ret (fun l2 -> if l2 = 0 then M.fail "Division by zero" else M.ret (l1 / l2))
end

(* First with a natural semantics *)
open MakeInterpreter(Input(Monads.ID))

let s x y = Binop (Sub, x, y)
let a x y = Binop (Add, x, y)
let d x y = Binop (Div, x, y)
let m x y = Binop (Mult, x, y)

let () =
  let three = Const 3 in
  let twelve = Const 12 in
  let t1 = s (a three (d (m twelve twelve) three)) twelve in (* (3 + ((12 * 12) / 3)) - 12 *)
  let v = eval t1 in
  assert (v = (3 + ((12 * 12) / 3)) - 12)

(* Then with a collecting semantics *)

open MakeInterpreter(Input(Monads.List))

let s x y = Binop (Sub, x, y)
let a x y = Binop (Add, x, y)
let d x y = Binop (Div, x, y)
let m x y = Binop (Mult, x, y)

let () =
  let three = Const 3 in
  let twelve = Const 12 in
  let t1 = s (a three (d (m twelve twelve) three)) twelve in (* (3 + ((12 * 12) / 3)) - 12 *)
  let v = eval t1 in
  assert (v = [(3 + ((12 * 12) / 3)) - 12])

let () =
  let zero = Const 0 in
  let v = eval (d zero zero) in
  assert(v = [])
