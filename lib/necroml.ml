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
let wrap f x =
  let () = Printexc.record_backtrace true in
  try f x
  with e ->
    let stack = Printexc.get_backtrace () in
    let msg = Printexc.to_string e in
    Printf.eprintf "Fatal error: %s%s\n" msg stack ;
    exit 1
let wrap2 f x y = wrap (wrap f x) y

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
  \t-o [FILE]/ --output [FILE]\tOutputs the result in given file. If this
  option is not provided, the result is printed in stdout.\n")
  Sys.argv.(0) ; exit 1

type minus =
| Depend
| Output of string
| Interface of string

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
  let files, opt = parse (Array.to_list Sys.argv) in
  let f, d =
    begin match files with
    | [] -> usage ()
    | a :: [] when List.mem Depend opt -> a, Necro.find_deps a
    | _ :: _ when List.mem Depend opt -> usage ()
    | a :: q -> a, List.rev q
    end
  in
  let output_file =
    let l = List.filter_map (function | Output sk -> Some sk | _ -> None) opt in
    begin match l with
    | [] -> None
    | a :: q -> if List.for_all ((=) a) q then Some a else usage ()
    end
  in
  let deps' = Necro.parse_and_type_list d in
  let deps = List.combine (List.map get_basename d) deps' in
  let sem = Necro.parse_and_type d f in
  let ml_ast =
    wrap2 Generator.generate_interpreter deps (get_basename f, sem) in
  let result = Mlprinter.print_ast ml_ast in
  Necro.Util.print_in_file result output_file

