module List = struct
	type 'a t = 'a list
	type ('a, 'b) arrow = 'a -> 'b t
	let ret x = [x]
	let rec branch l =
		List.concat (List.map (fun f -> f ()) l)
	let fail _ = []
  let rec union_two l = function
  | [] -> l
  | a :: q -> (if List.mem a l then (fun x -> x) else (List.cons a)) (union_two l q)
  let rec union = function
  | [] -> []
  | a :: q -> union_two a (union q)
  let bind l f = union (List.map f l)
end

module Cont = struct
  type fcont = string -> unit
  type 'a cont = 'a -> fcont -> unit
  type 'a t = 'a cont -> fcont -> unit
  type ('a, 'b) arrow = 'a -> 'b t
  let ret (x: 'a) = fun (k:'a cont) fcont -> k x fcont
  let bind (x: 'a t) (f: 'a -> 'b t) = fun k fcont -> x (fun v fcont' -> f v k fcont') fcont
  let fail s = fun k fcont -> fcont s
  let rec branch l = fun k fcont ->
    match l with
    | [] -> fcont "No branch matches"
    | b :: bs ->
      (b ()) k (fun () -> branch bs k fcont)
end

module ContPoly = struct
  type 'b fcont = string -> 'b
  type ('a,'b) cont = 'a -> 'b fcont -> 'b
  type 'a t = {cont: 'b. (('a,'b) cont -> 'b fcont -> 'b)}
  type ('a, 'b) arrow = 'a -> 'b t
  let ret (x: 'a) = { cont= fun k fcont -> k x fcont }
  let bind (x: 'a t) (f: 'a -> 'b t) : 'b t =
    { cont = fun k fcont -> x.cont (fun v fcont' -> (f v).cont k fcont') fcont }
  let fail s = { cont = fun k fcont -> fcont s }
  let rec branch l = { cont = fun k fcont ->
    match l with
    | [] -> fcont "No branch matches"
    | b :: bs -> (b ()).cont k (fun _ -> (branch bs).cont k fcont) }
end

module ID = struct
	exception Branch_fail of string
	type 'a t = 'a
	type ('a, 'b) arrow = 'a -> 'b t
	let ret x = x
	let rec branch l =
		begin match l with
		| [] -> raise (Branch_fail "No branch matches")
		| b1 :: bq ->
				try b1 () with Branch_fail _ -> branch bq
		end
	let fail s = raise (Branch_fail s)
	let bind x f = f x
end


module Rand = struct
	exception Branch_fail of string
	type 'a t = 'a
	type ('a, 'b) arrow = 'a -> 'b t
	let ret x = x
	let fail s = raise (Branch_fail s)
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
		| [] -> raise (Branch_fail "No branch matches")
		| l ->
				let (branch1, rest) = random_pick l in
				begin try
					branch1 ()
				with Branch_fail _ ->
					branch rest
				end
		end

end
