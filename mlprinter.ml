open Mlast
open Format

let fail () = ()

let print_com ff com =
	begin match com with
	| None -> ()
	| Some c ->
			fprintf ff "(**%s*)" c
	end

let print_com_nl ff com =
	begin match com with
	| None -> ()
	| Some _ -> fprintf ff "%a@," print_com com
	end

let print_space_com ff com =
	begin match com with
	| None -> ()
	| Some _ -> fprintf ff " %a" print_com com
	end

let print_with ff withes =
	let print_one_with ff (name, def) =
		fprintf ff
			" with module %s = %s"
			name
			(String.concat "." def)
	in
	Util.print_list ~sep:"" print_one_with ff withes

let print_from ff from =
	Option.iter (fun x -> fprintf ff "%s." x) from

let print_type_var ff v =
	fprintf ff "'%s" v

let rec monoline v =
	begin match v with
	| MLTVar (f, name) -> true
	| MLTConstr (f, c, None) -> true
	| MLTConstr (f, c, Some t) -> monoline t
	| MLTTop -> true
	| MLTTuple [] | MLTTuple [_] -> assert false
	| MLTTuple tl -> List.for_all monoline tl
	| MLTRecord (None, fl) -> List.for_all (fun (_, _, v) -> monoline v) fl
	| MLTRecord (Some t, fl) ->
			List.for_all (fun (_, _, v) -> monoline v) fl && monoline t
	| MLTFunc _ -> false
	| MLTFunction _ -> false
	| MLTField (t, f, n) -> monoline t
	| MLTApp (t, tl) -> List.for_all monoline (t::tl)
	| MLTLetIn (p, bound, body) -> false
	| MLTLetStarIn (p, bound, body) -> false
	| MLTMatchWith (t, l) -> false
	| MLTString s -> true
	| MLTList [] -> true
	| MLTList [t] -> monoline t
	| MLTList l -> false
	end

let rec print_type ff ty =
	begin match ty with
	| MLTyVar v -> print_type_var ff v
	| MLTyFrom (from, s, args) -> 
			begin match args with
			| []      ->
					fprintf ff "%a%s" print_from from s
			| a :: [] ->
					fprintf ff "%a %a%s" print_type_protect a print_from from s
			| args    ->
					fprintf ff "(%a) %a%s"
					(Util.print_list ~sep:", " print_type) args print_from from s
			end
	| MLTyBase (s, args) -> print_type ff (MLTyFrom (None, s, args))
	| MLTyUnit -> fprintf ff "unit"
	| MLTyString -> fprintf ff "string"
	| MLTyList ty -> fprintf ff "%a list" print_type_protect ty
	| MLTyArrow (tyi, tyo) ->
			begin match tyi with
			| MLTyArrow _ ->
					fprintf ff "%a -> %a" print_type_protect tyi print_type tyo
			| _ -> fprintf ff "%a -> %a" print_type tyi print_type tyo
			end
	| MLTyTuple tl ->
			let () = assert (List.length tl >= 2) in
			Util.print_list ~sep:" * " print_type_prod_comp ff tl
	end

and print_type_prod_comp ff ty =
	begin match ty with
	| MLTyVar _
	| MLTyBase (_, [])
	| MLTyFrom (_, _, [])
	| MLTyUnit
	| MLTyString
	| MLTyBase (_, _ :: _)
	| MLTyFrom (_, _, _ :: _)
	| MLTyList _ ->
			print_type ff ty
	| MLTyArrow _
	| MLTyTuple _ ->
			fprintf ff "(%a)" print_type ty
	end

and print_type_protect ff ty =
	begin match ty with
	| MLTyVar _
	| MLTyBase (_, [])
	| MLTyFrom (_, _, [])
	| MLTyUnit
	| MLTyString ->
			print_type ff ty
	| MLTyBase (_, _ :: _)
	| MLTyFrom (_, _, _ :: _)
	| MLTyList _
	| MLTyArrow _
	| MLTyTuple _ ->
			fprintf ff "(%a)" print_type ty
	end

let rec print_pattern ff p =
	begin match p with
	| MLPWild -> fprintf ff "_"
	| MLPVar x -> pp_print_string ff x
	| MLPConstr (f, c, None) -> fprintf ff "%a%s" print_from f c
	| MLPConstr (f, c, Some p) ->
			fprintf ff "%a%s %a" print_from f c print_pattern p
	| MLPTop -> fprintf ff "()"
	| MLPTuple pl ->
			let () = assert (List.length pl >= 2) in
			fprintf ff "(%a)" (Util.print_list ~sep:", " print_pattern) pl
	| MLPRecord fl ->
			let print_field ff (from, f, p) =
				fprintf ff "%a%s = %a" print_from from f print_pattern p
			in
			fprintf ff "{%a}" (Util.print_list ~sep:"; " print_field) fl
	end

and print_pattern_protect ff p =
	begin match p with
	| MLPConstr (_, _, Some _) ->
			fprintf ff "(%a)" print_pattern p
	| MLPWild | MLPVar _ | MLPConstr (_, _, None)
	| MLPTop | MLPTuple _ | MLPRecord _ ->
			print_pattern ff p
	end

let rec print_value ff v =
	begin match v with
	| MLTVar (f, name) ->
			fprintf ff "%a%s" print_from f name
	| MLTConstr (f, c, None) ->
			fprintf ff "%a%s" print_from f c
	| MLTConstr (f, c, Some t) ->
			fprintf ff "%a%s %a" print_from f c print_value_protect t
	| MLTTop -> Format.fprintf ff "()"
	| MLTTuple [] | MLTTuple [_] -> assert false
	| MLTTuple tl ->
			fprintf ff "(%a)" (Util.print_list ~sep:", " print_value) tl
	| MLTRecord (None, fl) ->
			let print_field ff (f, n, v) =
				Format.fprintf ff "%a%s = %a" print_from f n print_value v
			in
			fprintf ff "{%a}" (Util.print_list ~sep:"; " print_field) fl
	| MLTRecord (Some t, fl) ->
			let print_field ff (f, n, v) =
				Format.fprintf ff "%a%s = %a" print_from f n print_value v
			in
			fprintf ff
				"{%a with %a}" print_value t
				(Util.print_list ~sep:"; " print_field) fl
	| MLTFunc (p, t) ->
			fprintf ff "function %a ->@,%a" print_pattern_protect p print_value t
	| MLTFunction l ->
			fprintf ff "function %a"
			(pp_print_list (fun ff (p, t) ->
				fprintf ff "| %a ->@,%a@," print_pattern_protect p print_value t)) l
	| MLTField (t, f, n) ->
			fprintf ff "%a.%a%s" print_value t print_from f n
	| MLTApp (t, tl) ->
			fprintf ff "@[<v 2>%a %a@]" print_value_protect t
			(Util.print_list ~sep:" " print_value_protect) tl
	| MLTLetIn (p, bound, body) ->
			fprintf ff "@[<v 2>let %a =%ain@,%a"
			print_pattern p
			print_bind bound
			print_value body
	(* fold [let* p = M.ret y in …] into [let p = y in …] *)
	| MLTLetStarIn (p, MLTApp (MLTVar (Some "M", "ret"), [arg]), body) ->
			print_value ff (MLTLetIn (p, arg, body))
	| MLTLetStarIn (p, bound, body) ->
			fprintf ff "@[<v 2>let* %a =%ain@,%a"
			print_pattern p
			print_bind bound
			print_value body
	| MLTMatchWith (t, l) ->
			let space ff res =
				if monoline res
				then Format.fprintf ff " "
				else Format.fprintf ff "@,"
			in
			let print_branch ff (pat, res) =
				fprintf ff "@[<v 4>| %a ->%a%a@]" print_pattern pat space res print_value res
			in
			fprintf ff "begin match %a with@,%a@,end"
			print_value t (pp_print_list print_branch) l
	| MLTString s ->
			fprintf ff "\"%s\"" s
	| MLTList [] -> fprintf ff "[]"
	| MLTList [v] when monoline v -> fprintf ff "[%a]" print_value v
	| MLTList l ->
			let print_value' ff v =
				begin match v with
				| MLTFunc _ -> fprintf ff "@[<v 2>(%a)@]" print_value v
				| _ -> print_value ff v
				end
			in
			fprintf ff "[@,%a]"
			(Format.pp_print_list ~pp_sep:(fun ff () -> Format.fprintf ff " ;@,")
				(fun ff -> Format.fprintf ff "%a" print_value')) l
	end

and print_bind ff v =
	if monoline v
	then fprintf ff " %a@] " print_value v
	else fprintf ff "@,%a@]@," print_value v

and print_value_protect ff v =
	begin match v with
	| MLTVar _ | MLTConstr (_, _, None) | MLTTop | MLTTuple _ | MLTRecord _
	| MLTField _ | MLTMatchWith _ | MLTString _ | MLTList _ ->
			print_value ff v
	| MLTConstr (_, _, Some _) | MLTFunc _ | MLTFunction _ | MLTApp _ | MLTLetIn _
	| MLTLetStarIn _ ->
			Format.fprintf ff "(%a)" print_value v
	end

let print_poly_type ff (MLTyPoly (ta, ty)) =
	begin match ta with
	| [] -> print_type ff ty
	| _ -> fprintf ff "%a. %a"
		(Util.print_list ~sep:" " print_type_var) ta print_type ty
	end

let print_field_def ff (name, ty, com) =
		fprintf ff "%s: %a ;%a" name print_type ty print_space_com com

let print_constr_def ff (name, ty, com) =
	begin match ty with
	| None ->
			fprintf ff "| %s%a" name print_space_com com
	| Some (MLTyTuple _ | MLTyArrow _ as ty) ->
			fprintf ff "| %s of (%a)%a" name print_type ty print_space_com com
	| Some ty ->
			fprintf ff "| %s of %a%a" name print_type ty print_space_com com
	end

let print_type_def beg ff def =
	begin match def with
	| TDVariant (com, a, name, cl) ->
			begin match cl with
			| [] ->
					fprintf ff "%a@[<v>%s %a = |@]"
						print_com_nl com beg
						print_type (MLTyBase (name, List.map (fun x -> MLTyVar x) a))
			| _ ->
					fprintf ff "%a@[<v>%s %a =@,%a@]"
						print_com_nl com beg
						print_type (MLTyBase (name, List.map (fun x -> MLTyVar x) a))
						(pp_print_list print_constr_def) cl
			end
	| TDRecord (com, a, name, fl) ->
			begin match fl with
			(* records with no fields are indistinguishable to tuples with no
			   components, and are therefore treated as such *)
			| [] -> assert false
			| _ ->
					fprintf ff "%a@[<v>%s %a = {@,%a@,}@]"
						print_com_nl com beg
						print_type (MLTyBase (name, List.map (fun x -> MLTyVar x) a))
						(pp_print_list print_field_def) fl
			end
	| TDAlias (com, a, name, ty) ->
			fprintf ff
			"%a@[<v>%s %a = %a@]"
				print_com_nl com
				beg
				print_type (MLTyBase (name, List.map (fun x -> MLTyVar x) a))
				print_type ty
	end

let print_type_defs ff defs =
	begin match defs with
	| [] -> assert false
	| [def] -> print_type_def "type" ff def
	| def1 :: defs ->
			fprintf ff "%a@,%a"
				(print_type_def "type") def1
				(pp_print_list (print_type_def "and")) defs
	end

let rec print_val_def beg ff (name, com, args, ty, def) =
	begin match ty, def with
	| None, MLTFunc (MLPVar x, t) ->
			print_val_def beg ff (name, com, args @ [x], ty, t)
	| _ ->
			let post_equal ff =
				if monoline def
				then Format.fprintf ff " "
				else Format.fprintf ff "@,"
			in
			begin match ty with
			| None ->
				fprintf ff "%a@[<v 2>%s %s %a=%t%a@]"
					print_com_nl com beg name
					(Util.print_list ~sep:"" (fun ff x ->
						Format.fprintf ff "%s " x)) args
					post_equal
					print_value def
			| Some ty ->
				fprintf ff "%a@[<v 2>%s %s: %a %a =%t%a@]"
					print_com_nl com beg name print_poly_type ty
					(Util.print_list ~sep:" " pp_print_string) args
					post_equal
					print_value def
			end
	end

let print_val_defs ff defs =
	begin match defs with
	| [] -> assert false
	| [def] -> print_val_def "let rec" ff def
	| def1 :: defs ->
			fprintf ff "%a@,%a"
				(print_val_def "let rec") def1
				(pp_print_list (print_val_def "and")) defs
	end

let rec print_module_type_tld ff decl =
	begin match decl with
	| MTTypeDecl (com, args, name) ->
			let print_args ff args =
				let print_arg ff arg =
					begin match arg with
					| Some v -> fprintf ff "'%s" v
					| None -> fprintf ff "_"
					end
				in
				begin match args with
				| [] -> ()
				| [ arg ] ->
						fprintf ff "%a " print_arg arg
				| _ ->
						fprintf ff
							"(%a) " (Util.print_list ~sep:", " print_arg) args
				end
			in
			fprintf ff
				"%atype %a%s" print_com_nl com print_args args name
	| MTValDecl (com, name, ty) ->
			fprintf ff "%aval %s: %a" print_com_nl com name print_type ty
	| MTTypeDefs defs -> print_type_defs ff defs
	| MTModule (name, path, withes) ->
			fprintf ff
				"module %s: %s%a"
				name (String.concat "." path) print_with withes
	| MTInclude name ->
			fprintf ff "include %s" name
	| MTBlock decls ->
			pp_print_list print_module_type_tld ff decls
	end

let rec print_functor_tld ff decl =
	begin match decl with
	| MTypeDefs defs -> print_type_defs ff defs
	| MRecValDefs defs -> print_val_defs ff defs
	| MValDef def -> print_val_def "let" ff def
	| MInclude path -> fprintf ff "include %s" (String.concat "." path)
	| MException (name, ty) ->
			fprintf ff "exception %s of %a" name print_type ty
	| MOpen path -> fprintf ff "open %s" (String.concat "." path)
	| MModuleAlias (name, def) ->
			fprintf ff
				"module %s = %s"
				name (String.concat "." def) 
	| MBlock decls ->
			pp_print_list print_functor_tld ff decls
	end

let rec clean_moduletype body =
	begin match body with
	| [] -> []
	| MTBlock [] :: q -> clean_moduletype q
	| MTBlock x :: q ->
			let x = clean_moduletype x in
			begin match x with
			| [] -> clean_moduletype q
			| [tld] -> clean_moduletype (tld :: q)
			| _ -> MTBlock x :: clean_moduletype q
			end
	| MTTypeDefs [] :: q -> clean_moduletype q
	| a :: q -> a :: clean_moduletype q
	end

let rec clean_module body =
	begin match body with
	| [] -> []
	| MBlock [] :: q -> clean_module q
	| MBlock x :: q ->
			let x = clean_module x in
			begin match x with
			| [] -> clean_module q
			| [tld] -> clean_module (tld :: q)
			| _ -> MBlock x :: clean_module q
			end
	| MTypeDefs [] :: q -> q
	| MRecValDefs [] :: q -> q
	| a :: q -> a :: clean_module q
	end

let print_tld ff decl =
	begin match decl with
	| ModuleType (com, name, body) ->
			let body = clean_moduletype body in
			fprintf ff
				"%a@[<v 2>module type %s = sig@,%a@]@.end"
				print_com_nl com
				name
				(pp_print_list
					~pp_sep:(fun ff () -> fprintf ff "\n@,")
					print_module_type_tld) body
	| Functor (com, name, args, body) ->
			let body = clean_module body in
			let print_arg ff (name, modpath, withes) =
				fprintf ff
					"(%s: %s%a) "
					name
					(String.concat "." modpath)
					print_with withes
			in
			fprintf ff
				"%a@[<v 2>module %s %a= struct@,%a@]@.end"
				print_com_nl com
				name
				(Util.print_list ~sep:"" print_arg) args
				(pp_print_list
					~pp_sep:(fun ff () -> fprintf ff "\n@,")
					print_functor_tld) body
	| Comment c ->
			fprintf ff "(**%s*)" c
	end

let print_ast ast =
	let ff = str_formatter in
	let () =
		Util.print_list ~sep:"\n\n" print_tld ff ast
	in
	flush_str_formatter ()

