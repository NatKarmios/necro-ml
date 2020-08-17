open Arith_list

module Types = struct
	type literal = int
	type value = int
end

module Input = struct
	include Flow(Types)
	let litToVal l = [l]
	let add l1 = [function l2 -> [l1 + l2]]
	let sub l1 = [function l2 -> [l1 - l2]]
	let mult l1 = [function l2 -> [l1 * l2]]
	let div l1 = [function l2 -> if l2 = 0 then [] else [l1 / l2]]
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
  assert (v = [(3 + ((12 * 12) / 3)) - 12])

let () =
  let zero = Const 0 in
  let v = expr (d zero zero) in
  assert(v = [])
