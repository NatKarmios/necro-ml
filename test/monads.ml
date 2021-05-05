module ID = struct
	type kind = | Anon | Spec of string | Unspec of string
	exception Branch_fail of string
	type 'a t = 'a
	let ret x = x
	let rec branch l =
		begin match l with
		| [] -> raise (Branch_fail "No branch matches")
		| b1 :: bq ->
				try b1 () with Branch_fail _ -> branch bq
		end
	let fail s = raise (Branch_fail s)
	let bind x f = f x
	let apply _ f x = f x
end

module Rand = struct
	type kind = | Anon | Spec of string | Unspec of string
	exception Branch_fail of string
	type 'a t = 'a
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
	let apply _ f x = f x
end

module List = struct
	type kind = | Anon | Spec of string | Unspec of string
	type 'a t = 'a list
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
	let apply _ f x = f x
end

module Cont = struct
	type kind = | Anon | Spec of string | Unspec of string
	type fcont = string -> unit
	type 'a cont = 'a -> fcont -> unit
	type 'a t = 'a cont -> fcont -> unit
	let ret (x: 'a) = fun (k:'a cont) fcont -> k x fcont
	let bind (x: 'a t) (f: 'a -> 'b t) = fun k fcont -> x (fun v fcont' -> f v k fcont') fcont
	let fail s = fun k fcont -> fcont s
	let rec branch l = fun k fcont ->
		match l with
		| [] -> fcont "No branch matches"
		| b :: bs ->
			(b ()) k (fun _ -> branch bs k fcont)
	let apply _ f x = f x
end

module ContPoly = struct
	type kind = | Anon | Spec of string | Unspec of string
	type 'b fcont = string -> 'b
	type ('a,'b) cont = 'a -> 'b fcont -> 'b
	type 'a t = {cont: 'b. (('a,'b) cont -> 'b fcont -> 'b)}
	let ret (x: 'a) = { cont= fun k fcont -> k x fcont }
	let bind (x: 'a t) (f: 'a -> 'b t) : 'b t =
		{ cont = fun k fcont -> x.cont (fun v fcont' -> (f v).cont k fcont') fcont }
	let fail s = { cont = fun k fcont -> fcont s }
	let rec branch l = { cont = fun k fcont ->
		match l with
		| [] -> fcont "No branch matches"
		| b :: bs -> (b ()).cont k (fun _ -> (branch bs).cont k fcont) }
	let apply _ f x = f x
end

module Bfs = struct
	type kind = | Anon | Spec of string | Unspec of string
	type 'a t =
		| Ret : 'a -> 'a t
		| Bind : ('b t * ('b -> 'a t)) -> 'a t
		| Branch: (unit -> 'a t) Queue.t -> 'a t
		| Apply : ('a -> 'b t) * 'a -> 'b t
	let ret x = Ret x
	let branch l =
		let q = Queue.create () in
		let () = Stdlib.List.iter (fun x -> Queue.push x q) l in
		Branch q
	let fail _ = branch []
	let bind x f = Bind (x, f)
	let rec eval_step : type a. a t -> a t =
		let open Queue in
		begin function
		| Ret x -> Ret x
		| Bind (Ret x, f) -> f x
		| Bind (x, f) -> Bind (eval_step x, f)
		| Apply (f, x) -> f x
		| Branch l when is_empty l -> Branch l
		| Branch l ->
				let x = Queue.take l in
				begin match x () with
				| Ret x -> Ret x
				| Branch l' -> Queue.transfer l' l; Branch l
				| w -> Queue.add (fun () -> eval_step w) l; Branch l
				end
		end
	let rec repeat i f x =
		if i < 0 then x else
			repeat (i - 1) f (f x)
	let rec eval: type a. ?i:int -> a t -> a =
		fun ?(i=1) ->
			begin function
			| Ret x -> x
			| Branch l when Queue.is_empty l -> failwith "No result"
			| y -> let y' = repeat i eval_step y in eval ~i:(i + 1) y'
			end
	let apply _ f x = Apply (f, x)
end
