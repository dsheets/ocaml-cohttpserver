(*
 * Copyright (c) 2010-2012 Anil Madhavapeddy <anil@recoil.org>
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

open Lwt
open Printf

let server_t =
  let open Cohttpd.Server in
  let port = 8080 in
  let spec = { default_spec with callback=Dispatch.t; port=port; auto_close=true } in
  main spec

let cancel_t =
  lwt () = Lwt_unix.sleep 10.0 in
  exit 0

let req_num = ref 0

let rec request_t () =
  lwt () = Lwt_unix.sleep 1.0 in
  incr req_num;
  let uri = Uri.of_string "http://localhost:8080/" in
  lwt hdrs, body = Cohttpd.Client.get uri in
  printf "#%d:\n%!" !req_num;
  List.iter (fun (k,v) -> printf "  %.20s: %s\n" k v) hdrs;
  printf "%s\n\n%!" body;
  request_t ()

let _ =
  Lwt_main.run (server_t <?> cancel_t <?> (request_t ()))
