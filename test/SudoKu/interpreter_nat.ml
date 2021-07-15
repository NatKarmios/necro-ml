open Nat

module Input = struct
	include Unspec(Monads.ContPoly)(struct end)(Interpreter_boolean)
end

module Interp = MakeInterpreter(Input)(Interpreter_boolean)
include Interp

