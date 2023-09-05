(**************************************************************************)
(*                                                                        *)
(*                             Necro Library                              *)
(*                                                                        *)
(*                 Victoire Noizet, projet Épicure, Inria                 *)
(*                                                                        *)
(*   Copyright 2022 INRIA.                                                *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU General Public License version 3 as described in the file    *)
(*   LICENSE.                                                             *)
(*                                                                        *)
(**************************************************************************)

type comment = string
type module_name = string
type mod_path = module_name list
type withes = (module_name * mod_path) list
type modarg = string * mod_path * withes
type modargs = modarg list
type arg = string
type args = arg list
type type_name = string
type term_name = string
type from = module_name list

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
	| MLTDVariant of comment option * args * type_name * constr list
	| MLTDRecord of comment option * args * type_name * field list
	| MLTDAlias of comment option * args * type_name * mltype

type mlpattern =
	| MLPWild
	| MLPVar of string
	| MLPConstr of string * mlpattern option
	| MLPTop
	| MLPTuple of mlpattern list
	| MLPRecord of (string * mlpattern) list
	| MLPOr of mlpattern * mlpattern

type mlterm =
	| MLTVar of from * string
	| MLTConstr of string * mlterm option
	| MLTTop
	| MLTTuple of mlterm list
	| MLTRecMake of (string * mlterm) list
	| MLTRecWith of mlterm * (string * mlterm) list
	| MLTFunc of mlpattern * mlterm
	| MLTFunction of (mlpattern * mlterm) list
	| MLTField of mlterm * string
	| MLTApp of mlterm * mlterm list
	| MLTLetIn of mlpattern * mlterm * mlterm
	| MLTLetStarIn of mlpattern * mlterm * mlterm
	| MLTMatchWith of mlterm * (mlpattern * mlterm) list
	| MLTString of string
	| MLTList of mlterm list

type module_type_tld =
	| MTBlock of module_type_tld list
	| MTTypeDecl of comment option * arg option list * type_name
	| MTValDecl of comment option * term_name * mltype
	| MTTypeDefs of type_def list
	| MTModule of module_name * mod_path * withes
	| MTInclude of module_name

type module_tld =
	| MBlock of module_tld list
	| MModule of string * module_tld list
	| MTypeDefs of type_def list
	| MTypeNonRec of type_def
	| MValDef of (term_name * comment option * args * polytype option * mlterm)
	| MRecValDefs of (term_name * comment option * args * polytype option * mlterm) list
	| MInclude of mod_path
	| MException of string * mltype
	| MOpenStruct of module_tld list
	| MOpen of mod_path
	| MModuleAlias of string * mod_path
	| MComment of string

type module_def = module_tld list
type module_type_def = module_type_tld list


type tld =
	| ModuleType of comment option * module_name * module_type_def
	| Functor of comment option * module_name * modargs * module_def
	| Comment of comment

type ast = tld list
