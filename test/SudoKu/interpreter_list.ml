open List

module Input = struct
	include Unspec(Monads.ContPoly)(struct
	end)(Interpreter_boolean)(Interpreter_nat)
end

module Interp = MakeInterpreter(Input)(Interpreter_boolean)(Interpreter_nat)
include Interp
