open Boolean

module Input = struct
	include Unspec(Monads.ContPoly)(struct end)
end

module Interp = MakeInterpreter(Input)
include Interp

