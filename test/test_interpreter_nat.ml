open Nat

module Unspec = Unspec(Monads.ID)(struct end)
module InterpNat = MakeInterpreter(Unspec)

open InterpNat

let zero = O
let succ n = S n

let rec nat_of_int = function
  | i when i < 0 -> assert false
  | 0 -> zero
  | n -> succ (nat_of_int (n-1))

let rec int_of_nat = function
  | O -> 0
  | S n -> int_of_nat n + 1

let one = succ zero
let two = succ one
let three = succ two
let four = succ three
let ten = nat_of_int 10

let () =
  let seventeen = (add ten (sub (mul three three) two)) in
  let sixteen = sub seventeen one in
  let () = assert(eq (pow four two) sixteen = True) in
  let () = assert(even sixteen = True) in
  let () = assert(max sixteen four = sixteen) in
  let () = assert(min sixteen four = four) in
  ()

let () =
	let one_rel = Pos one in
	let minus_one_rel = Neg one in
	let one_third = (one_rel, three) in
	let minus_one_fourth = (minus_one_rel, four) in
	(* 1/3 + -1/4 = - (1/3 * -1/4)        = 1/12  *)
	assert (addq one_third minus_one_fourth = oppq (mulq one_third minus_one_fourth))
