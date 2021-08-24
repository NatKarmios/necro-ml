type comment = string
type module_name = string
type mod_path = module_name list
type withes = (module_name * mod_path) list
type modarg = string * mod_path * withes
type modargs = modarg list
type arg = string
type args = arg list
type type_name = string
type val_name = string
type from = module_name option

type mltype =
	| MLTyVar of string
	| MLTyArrow of mltype * mltype
	| MLTyTuple of mltype list
	| MLTyBase of string * mltype list
	| MLTyFrom of from * string * mltype list
	| MLTyUnit | MLTyString
	| MLTyList of mltype

type polytype =
	| MLTyPoly of string list * mltype

type constr = string * mltype option * comment option
type field = string * mltype * comment option

type type_def =
	| TDVariant of comment option * args * type_name * constr list
	| TDRecord of comment option * args * type_name * field list
	| TDAlias of comment option * args * type_name * mltype

type mlpattern =
	| MLPWild
	| MLPVar of string
	| MLPConstr of from * string * mlpattern option
	| MLPTop
	| MLPTuple of mlpattern list
	| MLPRecord of (from * string * mlpattern) list

type mlterm =
	| MLTVar of from * string
	| MLTConstr of from * string * mlterm option
	| MLTTop
	| MLTTuple of mlterm list
	| MLTRecord of mlterm option * (from * string * mlterm) list
	| MLTFunc of mlpattern * mlterm
	| MLTFunction of (mlpattern * mlterm) list
	| MLTField of mlterm * from * string
	| MLTApp of mlterm * mlterm list
	| MLTLetIn of mlpattern * mlterm * mlterm
	| MLTLetStarIn of mlpattern * mlterm * mlterm
	| MLTMatchWith of mlterm * (mlpattern * mlterm) list
	| MLTString of string
	| MLTList of mlterm list

type module_type_tld =
	| MTBlock of module_type_tld list
	| MTTypeDecl of comment option * arg option list * type_name
	| MTValDecl of comment option * val_name * mltype
	| MTTypeDefs of type_def list
	| MTModule of module_name * mod_path * withes
	| MTInclude of module_name

type module_tld =
	| MBlock of module_tld list
	| MTypeDefs of type_def list
	| MValDef of (val_name * comment option * args * polytype option * mlterm)
	| MRecValDefs of (val_name * comment option * args * polytype option * mlterm) list
	| MInclude of mod_path
	| MException of string * mltype
	| MOpen of mod_path
	| MModuleAlias of string * mod_path

type module_def = module_tld list
type module_type_def = module_type_tld list


type tld =
	| ModuleType of comment option * module_name * module_type_def
	| Functor of comment option * module_name * modargs * module_def
	| Comment of comment

type ast = tld list
