module Queue:
	sig
		type 'a queue
		val empty: 'a queue
		val is_empty: 'a queue -> bool
		val to_list: 'a queue -> 'a list
		val of_list: 'a list -> 'a queue
		val push: 'a * 'a queue -> 'a queue
		val pop: 'a queue -> 'a * 'a queue
		val append: 'a queue * 'a queue -> 'a queue
		val map: ('a -> 'b) -> 'a queue -> 'b queue
	end =
struct
	type 'a queue =
		{ outbox: 'a list; inbox: 'a list}
	let empty = {outbox=[]; inbox=[]}
	let is_empty {outbox; inbox} =
		outbox = [] && inbox = []
	let push (x, q) =
		{q with inbox = x::q.inbox}
	let pop q =
		begin match q.outbox with
		| a :: o ->
				(a, {q with outbox=o})
		| [] ->
				begin match Stdlib.List.rev q.inbox with
				| [] -> failwith "Empty queue"
				| a :: q -> (a, {outbox=q;inbox=[]})
				end
		end
	let to_list {inbox;outbox} =
		outbox @ Stdlib.List.rev inbox
	let of_list l =
		{inbox = []; outbox = l}
	let append (a, b) =
		{b with inbox = (to_list a) @ b.inbox}
	let map f {outbox;inbox} =
		{
			outbox = Stdlib.List.map f outbox;
			inbox = Stdlib.List.map f inbox }
end

module type MONAD = sig
  type 'a t
  val ret: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val branch: (unit -> 'a t) list -> 'a t
  val fail: unit -> 'a t
  val apply: ('a -> 'b t) -> 'a -> 'b t
  val extract: 'a t -> 'a
end


module ID = struct
	exception Branch_fail
	type 'a t = 'a
	let ret x = x
	let rec branch l =
		begin match l with
		| [] -> raise Branch_fail
		| b1 :: bq ->
				try b1 () with Branch_fail -> branch bq
		end
	let fail () = raise (Branch_fail)
	let bind x f = f x
	let apply f x = f x
	let extract x = x
end

let () =
	Random.self_init ()

let shuffle l =
	let lrand = Stdlib.List.map (fun c -> (Random.bits (), c)) l in
	Stdlib.List.sort compare lrand |> Stdlib.List.map snd

module Rand (M: MONAD) = struct
	include M
	let branch l =
		branch (shuffle l)
end

module List = struct
	type 'a t = 'a list
	let ret x = [x]
	let branch l =
		Stdlib.List.concat (Stdlib.List.map (fun f -> f ()) l)
	let fail _ = []
	let bind l f = Stdlib.List.concat (Stdlib.List.map f l)
	let apply f x = f x
	let extract x =
		begin match x with
		| a :: _ -> a
		| [] -> failwith "No result"
		end
end

module Cont = struct
	type fcont = unit -> unit
	type 'a cont = 'a -> fcont -> unit
	type 'a t = 'a cont -> fcont -> unit
	let ret (x: 'a) = fun (k:'a cont) fcont -> k x fcont
	let bind (x: 'a t) (f: 'a -> 'b t) =
		fun k fcont -> x (fun v fcont' -> f v k fcont') fcont
	let fail _ = fun _k fcont -> fcont ()
	let rec branch l = fun k fcont ->
		begin match l with
		| [] -> fcont ()
		| b :: bs -> (b ()) k (fun _ -> branch bs k fcont)
		end
	let apply f x = f x
	let extract (x: 'a t) =
		let y = ref None in
		let () = x (fun a _ -> y:=Some a) (fun () -> failwith "No result") in
		Option.get !y
end

module ContPoly = struct
	type 'b fcont = unit -> 'b
	type ('a,'b) cont = 'a -> 'b fcont -> 'b
	type 'a t = {cont: 'b. (('a,'b) cont -> 'b fcont -> 'b)}
	let ret (x: 'a) = { cont= fun k fcont -> k x fcont }
	let bind (x: 'a t) (f: 'a -> 'b t) : 'b t =
		{ cont = fun k fcont -> x.cont (fun v fcont' -> (f v).cont k fcont') fcont }
	let fail () = { cont = fun _k fcont -> fcont () }
	let rec branch l = { cont = fun k fcont ->
		match l with
		| [] -> fcont ()
		| b :: bs -> (b ()).cont k (fun _ -> (branch bs).cont k fcont) }
	let apply f x = f x
	let extract x =
		x.cont (fun a _ -> a) (fun () -> failwith "No result")
end

module Bfs = struct
	type 'a t =
		| Ret : 'a -> 'a t
		| Bind : ('b t * ('b -> 'a t)) -> 'a t
		| Branch: (unit -> 'a t) Queue.queue -> 'a t
		| Apply : ('a -> 'b t) * 'a -> 'b t
	let ret x = Ret x
	let branch l = Branch (Queue.of_list l)
	let fail _ = branch []
	let bind x f = Bind (x, f)
	let rec eval_step : type a. a t -> a t =
		begin function
		(* Evaluation over *)
		| Ret _ ->
				failwith "Impossible to evaluate any further"
		| Branch l when Queue.is_empty l ->
				failwith "Impossible to evaluate any further"
		(* Evaluation not over *)
		| Bind (Ret x, f) -> f x
		| Bind (Branch l, f) ->
				let branches = Queue.map (fun x () -> Bind (x (), f)) l in
				Branch branches
		| Bind (x, f) -> Bind (eval_step x, f)
		| Apply (f, x) -> f x
		| Branch l ->
				let (x, q) = Queue.pop l in
				begin match x () with
				| Ret x -> Ret x
				| Branch l' -> Branch (Queue.append (q, l'))
				| w -> Branch (Queue.push ((fun () -> eval_step w), q))
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

module BfsOptim = struct
	type 'a t =
		| Ret : 'a -> 'a t
		| Bind : ('b t * ('b -> 'a t)) -> 'a t
		| Branch: (unit -> 'a t) Queue.queue -> 'a t
		| Apply : ('a -> 'b t) * 'a -> 'b t
	let ret x = Ret x
	let branch l = Branch (Queue.of_list l)
	let fail _ = branch []
	let bind x f = Bind (x, f)
	let rec eval_step: type a. int -> a t -> a t =
		fun n -> if n = 0 then fun x -> x else
		begin function
		(* Evaluation over *)
		| Ret x -> Ret x
		| Branch l when Queue.is_empty l -> Branch l
		(* Evaluation not over *)
		| Bind (Ret x, f) -> eval_step (n-1) (f x)
		| Bind (Branch l, f) ->
				let branches = Queue.map (fun x () -> Bind (x (), f)) l in
				Branch branches
		| Bind (x, f) -> eval_step (n-1) (Bind (eval_step 1 x, f))
		| Apply (f, x) -> eval_step (n-1) (f x)
		| Branch l ->
				let (x, q) = Queue.pop l in
				begin match eval_step (n-1) (x ()) with
				| Ret x -> Ret x
				| Branch l' -> Branch (Queue.append (q, l'))
				| w ->
						Branch (Queue.push ((fun () -> eval_step 1 w), q))
				end
		end
	let rec eval_aux: type a. int -> a t -> a =
		fun n ->
		begin function
		| Ret x -> x
		| Branch l when Queue.is_empty l -> failwith "No result"
		| y -> let y' = eval_step n y in eval_aux (n+1) y'
		end
	let eval: type a. a t -> a =
		fun x -> eval_aux 1 x
	let apply f x = Apply (f, x)
	let extract x = eval x
end

module BfsYield = struct
	type 'a t =
		| Ret : 'a -> 'a t
		| Bind : ('b t * ('b -> 'a t)) -> 'a t
		| Branch: (unit -> 'a t) Queue.queue -> 'a t
		| Apply : ('a -> 'b t) * 'a -> 'b t
		| Yield: 'a * 'a t -> 'a t
	let ret x = Ret x
	let branch l = Branch (Queue.of_list l)
	let fail _ = branch []
	let bind x f = Bind (x, f)
	let rec eval_step : type a. a t -> a t =
		begin function
		(* Evaluation over *)
		| Ret _ | Yield _ ->
				failwith "Impossible to evaluate any further"
		| Branch l when Queue.is_empty l ->
				failwith "Impossible to evaluate any further"
		(* Evaluation not over *)
		| Bind (Ret x, f) -> f x
		| Bind (Branch l, f) ->
				let branches = Queue.map (fun x () -> Bind (x (), f)) l in
				Branch branches
		| Bind (x, f) -> Bind (eval_step x, f)
		| Apply (f, x) -> f x
		| Branch l ->
				let (x, q) = Queue.pop l in
				begin match x () with
				| Ret x -> Yield (x, Branch q)
				| Branch l' -> Branch (Queue.append (q, l'))
				| w -> Branch (Queue.push ((fun () -> eval_step w), q))
				end
		end
	let rec eval: type a. a t -> a * a t =
		begin function
		| Ret x -> (x, Branch Queue.empty)
		| Branch l when Queue.is_empty l -> failwith "No result"
		| Yield (x, t) -> (x, t)
		| y -> let y' = eval_step y in eval y'
		end
	let apply f x = Apply (f, x)
	let extract x = let (v, _) = eval x in v
	let yield x = eval x
end

(* We check that our modules implement the MONAD module type *)
module _ = (ID:MONAD)
module _ = (List:MONAD)
module _ = (Cont:MONAD)
module _ = (ContPoly:MONAD)
module _ = (Bfs:MONAD)
module _ = (BfsYield:MONAD)
module _ (M:MONAD) = (Rand(M):MONAD)
