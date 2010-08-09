(*
 * Copyright (c) 2009 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Unix

(* wrapper for realpath(2) *)
external realpath : string -> string = "unix_realpath"

(* repeat until End_of_file is raised *)
let repeat_until_eof fn =
   try while true do fn () done
   with End_of_file -> ()

(* Retrieve file extension , if any, or blank string otherwise *)
let get_extension ~filename =
  let rec search_dot i =
    if i < 1 || filename.[i] = '/' then None
    else if filename.[i] = '.' then Some (String.sub filename (i+1) (String.length filename - i - 1))
    else search_dot (i - 1) in
  search_dot (String.length filename - 1)

(* Walk directory and call walkfn on every file that matches extension ext *)
let walk_directory_tree ?ext walkfn root_dir =
  let rec walk dir =
    let dh = opendir dir in
    repeat_until_eof (fun () ->
      match readdir dh with
      | "." | ".." -> ()
      | f ->
          let n = Filename.concat dir f in
          if Sys.is_directory n then walk n
          else
            (match (get_extension f), ext with
            | (_, None) -> walkfn root_dir (String.sub n 2 (String.length n - 2))
            | (Some e, Some e') when e = e'  -> walkfn root_dir (String.sub n 2 (String.length n - 2))
            | _ -> ())
    );
    closedir dh in
  chdir root_dir;
  walk "."

open Fs
open Printf

(* output file to database *)
let output_file db root name =
  printf "%s\n%!" name;
  let filename = Filename.concat root name in
  let ch = open_in filename in
  let len = (Unix.stat filename).Unix.st_size in
  let buf = Buffer.create 1024 in
  (try Buffer.add_channel buf ch len with _ -> ());
  close_in ch;
  eprintf "pwd: %s\n%!" (Sys.getcwd ());
  (try
    t_save db { name=name; body=(Buffer.contents buf) };
   with e -> (eprintf "YYYY\n%!"; raise e);
  )
  
open Arg
open Printf

let _ =
  let dirs = ref [] in
  let ext = ref None in
  let db = ref "crunch.db" in
  let spec = [
      ("-ext", String (fun e -> ext := Some e), "filter only these extensions");
      ("-db", Set_string db, "output database name");
    ] in
  parse spec (fun s -> dirs := (realpath s) :: !dirs) 
    (sprintf "Usage: %s [-ext <filter extension>] -db <dbname> <dir1> <dir2> ..." Sys.argv.(0));
  let ext = !ext in
  let db = try
    Fs.t_init !db 
    with e -> (eprintf "XXXXXXX\n%!"; raise e)
  in
  List.iter (walk_directory_tree ?ext (output_file db)) !dirs
