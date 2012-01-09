(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
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

let uris = [
  "https://www.google.com"
]

let client_t = 
  Lwt_list.iter_s (fun uri_str ->
    let uri = Uri.of_string uri_str in
    printf "GET %s\n" uri_str;
    printf "GET %s\n" (Uri.to_string uri);
    lwt hdrs, body = Cohttpd.Client.get uri in 
    List.iter (fun (k,v) -> printf "  %s: %s\n" k v) hdrs;
    printf "%s\n\n%!" body;
    return ()
  ) uris

let _ =
  Lwt_main.run client_t
