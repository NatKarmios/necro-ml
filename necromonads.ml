module type MONAD = sig
  type 'a t
  val ret: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val branch: (unit -> 'a t) list -> 'a t
  val fail: string -> 'a t
  val apply: ('a -> 'b t) -> 'a -> 'b t
  val extract: 'a t -> 'a
end


module ID = struct
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
	let apply f x = f x
	let extract x = x
end

let shuffle l =
	let () = Random.self_init () in
	let lrand = List.map (fun c -> (Random.bits (), c)) l in
	List.sort compare lrand |> List.map snd

module Rand (M: MONAD) = struct
	include M
	let branch l =
		branch (shuffle l)
end

module List = struct
	type 'a t = 'a list
	let ret x = [x]
	let rec branch l =
		Stdlib.List.concat (Stdlib.List.map (fun f -> f ()) l)
	let fail _ = []
	let rec union_two l = function
	| [] -> l
	| a :: q -> (if Stdlib.List.mem a l then (fun x -> x) else (Stdlib.List.cons a)) (union_two l q)
	let rec union = function
	| [] -> []
	| a :: q -> union_two a (union q)
	let bind l f = union (Stdlib.List.map f l)
	let apply f x = f x
	let extract x =
		begin match x with
		| a :: q -> a
		| [] -> failwith "No result"
		end
end

module Cont = struct
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
	let apply f x = f x
	let extract (x: 'a t) =
		let y = ref None in
		let () = x (fun a _ -> y:=Some a) (fun s -> failwith s) in
		Option.get !y
end

module ContPoly = struct
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
	let apply f x = f x
	let extract x =
		x.cont (fun a _ -> a) (fun s -> failwith s)
end

module Bfs = struct
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
	let rec eval: type a. a t -> a =
		begin function
		| Ret x -> x
		| Branch l when Queue.is_empty l -> failwith "No result"
		| y -> let y' = eval_step y in eval y'
		end
	let apply f x = Apply (f, x)
	let extract x = eval x
end
