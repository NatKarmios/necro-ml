open Arith

module Types = struct
  type literal = int
  type value = int
end

module Input = struct
	include Flow(Types)
  let litToVal l = l
  let add l1 l2 = (l1 + l2)
  let sub l1 l2 = (l1 - l2)
  let mult l1 l2 = (l1 * l2)
  let div l1 l2 =
    if l2 <> 0 then (l1 / l2) else raise (Branch_fail "Division by zero")
end

module InterpArith = MakeInterpreter(Input)

open InterpArith

let s x y = Binop (Sub, x, y)
let a x y = Binop (Add, x, y)
let d x y = Binop (Div, x, y)
let m x y = Binop (Mult, x, y)

let () =
  let three = Const 3 in
  let twelve = Const 12 in
  let t1 = s (a three (d (m twelve twelve) three)) twelve in (* (3 + ((12 * 12) / 3)) - 12 *)
  let v = expr t1 in
  assert (v = (3 + ((12 * 12) / 3)) - 12)

