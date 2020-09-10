open Pcf

module SMap = Map.Make(String)

module M = Monads.ID

module rec T : sig
		type ident = string
		type lit = int
		type nonrec int = int
		type env = value SMap.t
		and value =
			| Int of int
			| Clos of ident * Unspec(M)(T).lterm * env
	end =
	struct
  type ident = string
  type lit = int
  type nonrec int = int
  type env = value SMap.t
  and value =
    | Int of int
    | Clos of ident * Unspec(M)(T).lterm * env
end

module F = struct
	include T
	include Unspec(M)(T)
  let initial_env () = SMap.empty

  let mkClos (x, t, e) = Clos (x, t, e)
  let mkInt d = Int d
  let getClos = function
    | Int _ -> M.fail "getClos error"
    | Clos (x, t, e) -> (x, t, e)
  let getInt = function
    | Int i -> i
    | Clos _ -> M.fail "getInt error"

  let extEnv (env, x, v) = SMap.add x v env
  let getEnv (x, env) =
    try SMap.find x env
    with _ -> M.fail "getEnv error"

  let intOfLit v = v
  let plus (x, y) = x + y
  let isZero d = if d <> 0 then M.fail "isZero error"
  let isNotZero d = if d = 0 then M.fail "isNotZero error"
end

module I = MakeInterpreter(F)
open I
open F

let let_ n v body = App (Lam (n, body), v)

(* Fixpoint combinator: λf (λx f (λv (x x v)) (λx f (λv (x x v)))) *)
let ycomb = (Lam ("f", let u = Lam ("x", App (Var "f", Lam ("v", App (App (Var "x", Var "x"), Var "v")))) in App (u, u)))

let t0 =
  (* let z = Y (λf· λx· x) in z 42 *)
  let_ "Y" ycomb
    (let_ "z" (App (Var "Y", (Lam ("z", Lam ("x", Var "x")))))
       (App (Var "z", Const 42)))

let () = assert(eval (initial_env (), t0) = Int 42)

let t1 =
  (* let rec z x = if x = 0 then 0 else z (x - 1)
   * in z 42 *)
  let_ "Y" ycomb
    (let_ "z" (App (Var "Y", (Lam ("z", Lam ("x", IfZero (Var "x", Const 0, App (Var "z", Plus (Var "x", Const (-1)))))))))
       (App (Var "z", Const 42)))

let () = assert(eval (initial_env (), t1) = Int 0)

(* The evaluation of this expression generates a stack overflow *)
let t2 =
  (* let rec z x = if z = 0 then 0 else z (x - 1)
   * in z (-42) *)
  let_ "Y" ycomb
    (let_ "z" (App (Var "Y", (Lam ("z", Lam ("x", IfZero (Var "x", Const 0, App (Var "z", Plus (Var "x", Const (-1)))))))))
       (App (Var "z", Const (-42))))
