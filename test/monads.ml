module List = struct
	type 'a t = 'a list
	type ('a, 'b) arrow = 'a -> 'b t
	let ret x = [x]
	let rec branch l =
		List.concat (List.map (fun f -> f ()) l)
	let fail () = branch []
  let rec union_two l = function
  | [] -> l
  | a :: q -> (if List.mem a l then (fun x -> x) else (List.cons a)) (union_two l q)
  let rec union = function
  | [] -> []
  | a :: q -> union_two a (union q)
  let bind l f = union (List.map f l)
end


module ID = struct
	exception Branch_fail
	type 'a t = 'a
	type ('a, 'b) arrow = 'a -> 'b t
	let ret x = x
	let rec branch l =
		begin match l with
		| [] -> raise Branch_fail
		| b1 :: bq ->
				try b1 () with Branch_fail -> branch bq
		end
	let fail () = branch []
	let bind x f = f x
end


module Rand = struct
	exception Branch_fail
	type 'a t = 'a
	type ('a, 'b) arrow = 'a -> 'b t
	let ret x = x
	let rec branch l =
		begin match l with
		| [] -> raise Branch_fail
		| b1 :: bq ->
				try b1 () with Branch_fail -> branch bq
		end
	let fail () = branch []
	let bind x f = f x

		let rec extract pick l =
		begin match pick, l with
		| _, [] -> assert false
		| n, a :: q when n < 0 -> assert false
		| 0, a :: q -> a, q
		| n, a :: q ->
				let extracted, rest = extract (n - 1) q in
				extracted, a :: rest
		end
	let random_pick =
		begin function
		| [] -> assert false
		| l ->
				let len = Stdlib.List.length l in
				let () = Random.self_init () in
				let pick = Random.int len in
				extract pick l
		end
	let rec branch =
		begin function
		| [] -> raise Branch_fail
		| l ->
				let (branch1, rest) = random_pick l in
				begin try
					branch1 ()
				with Branch_fail ->
					branch rest
				end
		end

end
