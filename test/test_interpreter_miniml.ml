open Miniml

module SMap = Map.Make(String)

module rec T : (sig
	type ident = string
	type lit = int
	type nonrec int = int

	type env = value SMap.t
	and value =
		| VInt of int
		| VClos of ident * Flow(T).mlterm * env
		| VConstr of string * value Flow(T).list
end)
= struct
	type ident = string
	type lit = int
	type nonrec int = int

	type env = value SMap.t
	and value =
		| VInt of int
		| VClos of ident * Flow(T).mlterm * env
		| VConstr of string * value Flow(T).list
end

module F = struct
	include T
	module FF = Flow(T)
	include FF
	let rec of_list = function
		| [] -> FF.Nil
		| a::q -> FF.Cons (a, of_list q)
	let rec to_list = function
		| Nil -> []
		| Cons (a, q) -> a :: to_list q

	let initial_env () = SMap.empty

	let mkClos (x, t, e) = VClos (x, t, e)
	let mkInt d = VInt d
	let getClos = function
		| VClos (x, t, e) -> (x, t, e)
		| _ -> raise (Branch_fail "Not a closure")
	let getInt = function
		| VInt i -> i
		| _ -> raise (Branch_fail "Not an integer")
	let mkConstr (name, l) = VConstr (name, l)
	let isConstrWithName (name, v) = match v with
		| VConstr (n, l) when n = name -> l
		| _ -> raise (Branch_fail ("Not a constructor with name " ^ name))
	let isNotConstrWithName (name, v) = match v with
		| VConstr (n, _) when n = name ->
				raise (Branch_fail ("Is a constructor with name" ^ name))
		| _ -> ()

	let extEnv (env, x, v) = SMap.add x v env
	let getEnv (x, env) = try SMap.find x env with _ ->
		raise (Branch_fail (x ^ " not found in the environment"))

	let intOfLit v = v
	let plus (x, y) = (x + y)
	let isZero d = if d <> 0 then raise (Branch_fail "Is Not Zero")
	let isNotZero d = if d = 0 then raise (Branch_fail "Is Zero")

	let match_failure () = failwith "Match failure"
end

module I = MakeInterpreter(F)
open T
open F
open I

let let_ n v body = App (Lam (n, body), v)
let ycomb = Lam ("f", let u = Lam ("x", App (Var "f", Lam ("v", App (App (Var "x", Var "x"), Var "v")))) in App (u, u))
let letrec_ n v =
	let_ n (App (Var "Y", (Lam (n, v))))

let make_list l: mlterm =
	List.fold_right (fun v l -> Constr ("Cons", of_list [v; l])) l (Constr ("Nil", Nil))

let tpcf =
	(* let z = λ x → if x = 0 then 0 else z (x - 1) in z 42 *)
	let_ "Y" ycomb (
		letrec_ "z" (Lam ("x", IfZero (Var "x", Const 0, App (Var "z", Plus (Var "x", Const (-1))))))
			(App (Var "z", Const 42))
	)

let () = assert(eval (initial_env (), tpcf) = VInt 0)

let t0 =
	(* let list_sum = λ l → match l with | Nil -> 0 | Cons(v, l) -> v + list_sum l
	 * in list_sum (Cons (1, Cons (2, Cons (3, Nil)))) *)
	let_ "Y" ycomb (
		letrec_ "list_sum" (
			Lam ("l",
				Match (Var "l",
					of_list [
						(PConstr ("Nil", Nil),
							Const 0);
						(PConstr ("Cons", Cons (PVar "v", Cons (PVar "l", Nil))),
							Plus (Var "v", App (Var "list_sum", Var "l")))
					]
		)))
		(App (Var "list_sum", make_list [Const 1; Const 2; Const 3]))
	)

let () = assert(eval (initial_env (), t0) = VInt 6)

let t1 =
	(* let rev_append = λ l1 → λ l2 → match l1 with | [] -> l2 | v :: l -> rev_append l (v :: l2)
	 * in rev_append [1;2;3] [4;5;6] *)
	let_ "Y" ycomb (
		letrec_ "rev_append" (
			Lam ("l1", Lam ("l2",
				Match (Var "l1",
					of_list [
						(PConstr ("Nil", Nil),
							Var "l2");
						(PConstr ("Cons", Cons (PVar "v", Cons (PVar "l", Nil))),
							App (App (Var "rev_append", Var "l"), Constr ("Cons", Cons (Var "v", Cons (Var "l2", Nil)))));
					]
				)
		)))
		(App (App (Var "rev_append", make_list [Const 1; Const 2; Const 3]), make_list [Const 4; Const 5; Const 6]))
	)

let () = assert(eval (initial_env (), t1) =
	let rec make_val = function
		| [] -> VConstr("Nil", Nil)
		| a :: q -> VConstr("Cons", Cons(VInt a, Cons(make_val q, Nil)))
	in make_val [3; 2; 1; 4; 5; 6])
