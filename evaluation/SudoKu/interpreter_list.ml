open List

module Input(M:MONAD) = struct
	include Unspec(M)(struct end)(Interpreter_boolean.Interp(M))(Interpreter_nat.Interp(M))
end

module Interp(M:MONAD) = MakeInterpreter(Input(M))(Interpreter_boolean.Interp(M))(Interpreter_nat.Interp(M))
