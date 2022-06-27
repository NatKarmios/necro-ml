open Boolean

module Input(M:MONAD) = struct
	include Unspec(M)(struct end)
end

module Interp(M:MONAD) = MakeInterpreter(Input(M))

