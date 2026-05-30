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

(** * Auxiliary functions **)
let give_version () =
  Printf.printf "Necro ML, version: %s\n"
  begin match Build_info.V1.version () with
   | None -> "N/A"
   | Some v -> Build_info.V1.Version.to_string v
  end;
  exit 0


(** Wrap a function call to make the whole program fail with the right error code if an exception happens. **)
let wrap (thunk: 'a Lazy.t): 'a =
  begin try
    Lazy.force thunk
  with Necro.Errors.NecroError e ->
    Necro.Errors.report_error e ;
    exit 1
  | Sys_error x | Invalid_argument x  ->
    Format.eprintf "%s" x ;
    exit 1
  end


(* Print usage in stderr and exit *)
let usage () =
  Printf.eprintf
  ("Usage: %s [OPTION]... [FILE]...\n\n\
  Get an OCaml interpreter for the last given file. For every file, \
  the dependencies must precede in the list except if the -d option \
  is given.\n\n\
  Options:\n\
  \t-v / --version\tPrint version and exit.\n\
  \t-d / --find-deps\tOnly one file must be given. The dependencies are then \
  looked for in the cwd, and in the path of the given file.\n\
  \t-i [FILE]/ --interface [FILE]\tGet the given skeletal interface to be the
  signature for the interpreter\n\
  \t-w / --warnings\tChoose warnings to print, using a comma-separated list.\n\
  \t-a / --alltypes\tGenerate the AllTypes functor, only works for \
  first-order types.\n\  
  \t-o [FILE]/ --output [FILE]\tOutputs the result in given file. If this
  option is not provided, the result is printed in stdout.\n")
  Sys.argv.(0) ; exit 1

type minus =
| Depend
| Output of string
| Interface of string
| Warnings of string
| AllTypes

let parse l =
  let rec parse accu_files accu_options l =
    begin match l with
    | [] ->
        (accu_files, accu_options)
    | "-v" :: _ | "--version" :: _ ->
        give_version ()
    | "-d" :: q | "--find-deps" :: q ->
        parse accu_files (Depend :: accu_options) q
    | "-o" :: [] | "--output" :: [] -> usage ()
    | "-o" :: f :: q | "--output" :: f :: q ->
        parse accu_files (Output f :: accu_options) q
    | "-i" :: [] | "--interface" :: [] -> usage ()
    | "-i" :: f :: q | "--interface" :: f :: q ->
        parse accu_files (Interface f :: accu_options) q
    | "-w" :: x :: q | "--warnings" :: x :: q ->
        parse accu_files (Warnings x :: accu_options) q
    | "-a" :: q | "--alltypes" :: q ->
        parse accu_files (AllTypes :: accu_options) q
    | a :: q ->
        parse (a :: accu_files) accu_options q
    end
  in parse [] [] (List.tl l)

let get_basename filename =
  filename
  |> Filename.basename
  |> Filename.remove_extension

(** MAIN FUNCTION **)
let () =
  let files, options = parse (Array.to_list Sys.argv) in
  let f, d =
    begin match files with
    | [] -> usage ()
    | a :: [] when List.mem Depend options -> a, None
    | _ :: _ when List.mem Depend options -> usage ()
    | a :: q -> a, Some (List.rev q)
    end
  in
  let output_file =
    let l = List.filter_map (function
      | Output sk -> Some sk | _ -> None) options in
    begin match l with
    | [] -> None
    | a :: q -> if List.for_all ((=) a) q then Some a else usage ()
    end
  in
  let warn =
    let spec = List.filter_map (fun o ->
      begin match o with
      | Warnings x -> Some x
      | _ -> None
      end) options
    in
    begin match spec with
    | [] | ["all"]-> Necro.Errors.report_warning
    | ["none"] -> ignore
    | _ :: _ :: _ -> usage ()
    | [x] ->
        begin try
          let ignore = x
            |> String.split_on_char ','
            |> List.map int_of_string
            |> List.sort_uniq Int.compare
          in Necro.Errors.handle_warning ~ignore ()
        with _ ->
          usage ()
        end
    end
  in
  let depfiles, depdirs = match d with
  | Some depfiles -> depfiles, []
  | None -> [], [Filename.dirname f]
  in
  let sem, deps = wrap @@ lazy (Necro.parse_and_type_full ~warn ~depfiles ~depdirs f) in
  let ml_ast =
    wrap (lazy (Generator.generate_interpreter deps (get_basename f, sem) (List.mem AllTypes options))) in
  let result = Mlprinter.print_ast ml_ast in
  Necro.Printer.print_in_file result output_file

