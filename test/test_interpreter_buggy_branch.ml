open Buggy_branch

module Unspec = struct
  include Unspec(Monads.ContPoly)(struct end)

end

module Interp = MakeInterpreter(Unspec)

open Unspec
open Interp

let _ = (test ()).Monads.ContPoly.cont (fun x _ -> print_endline "success") (fun () -> failwith "no execution")
