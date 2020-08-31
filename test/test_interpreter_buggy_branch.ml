open Buggy_branch

module Unspec = struct
  include Unspec(Monads.Cont)(struct end)
end

module Interp = MakeInterpreter(Unspec)

open Unspec
open Interp

let _ = test () (fun x _ -> print_endline "success") (fun () -> failwith "no execution")
