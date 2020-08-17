open Lists_list

module InterpLists = MakeInterpreter(Flow(struct end))
open InterpLists

let rec of_list = function
  | [] -> Nil
  | a :: q -> Cons (a, of_list q)
let rec of_bool = function
  | true -> True
  | false -> False

let rec has_values l1 l2 = match l1 with
  | [] -> l2 = []
  | a :: q ->
      List.mem a l2 &&
      has_values q (List.filter (fun x -> x <> a) l2)


let () =
  let div2 x = [of_bool (x mod 2 == 0)] in
  let a = of_list [1; 2; 3; 4; 5; 6; 7; 8; 9] in
  let l = List.flatten (List.map (fun f -> f a) (find div2)) in
  assert (has_values [2;4;6;8] l)
