open Lists

module InterpLists = MakeInterpreter(Flow(struct end))
open InterpLists

let rec of_list = function
  | [] -> Nil
  | a :: q -> Cons (a, of_list q)

let rec of_bool = function
  | true -> True
  | false -> False

let () =
  let a = of_list [8; 3; 4; 1; 9; 2; 7; 5; 6] in
  let b = of_list [of_list [1; 2; 3]; of_list [4; 5; 6]; of_list [7; 8; 9]] in
  let le x y = of_bool (x < y) in
  assert (flatten b = sort le a)
