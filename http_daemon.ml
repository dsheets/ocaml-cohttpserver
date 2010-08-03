(*pp camlp4o -I `ocamlfind query lwt.syntax` pa_lwt.cmo *)

(*
  OCaml HTTP - do it yourself (fully OCaml) HTTP daemon

  Copyright (C) <2002-2005> Stefano Zacchiroli <zack@cs.unibo.it>
  Copyright (C) <2009> Anil Madhavapeddy <anil@recoil.org>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Library General Public License as
  published by the Free Software Foundation, version 2.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
  USA
*)

open Printf

open Cohttp
open Http_common
open Http_types
open Http_constants
open Http_parser

open Lwt

type conn_id = int
let string_of_conn_id = string_of_int

type daemon_spec = {
  address: string;
  auth: auth_info;
  callback: conn_id -> Http_request.request -> Lwt_io.output_channel Lwt.t -> unit Lwt.t;
  conn_closed : conn_id -> unit;
  port: int;
  root_dir: string option;
  exn_handler: exn -> Lwt_io.output_channel Lwt.t -> unit Lwt.t;
  timeout: int option;
  auto_close: bool;
}

exception Http_daemon_failure of string

  (** internal: given a status code and an additional body return a string
  representing an HTML document that explains the meaning of given status code.
  Additional data can be added to the body via 'body' argument *)
let control_body code body =
  let reason_phrase = Http_misc.reason_phrase_of_code code in
  sprintf
"<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">
<HTML><HEAD>
<TITLE>%d %s</TITLE>
</HEAD><BODY>
<H1>%d - %s</H1>%s
</BODY></HTML>"
    code reason_phrase code reason_phrase body

let respond_with response outchan =
  lwt outchan = outchan in
  Http_response.serialize response outchan

  (* Warning: keep default values in sync with Http_response.response class *)
let respond ?(body = "") ?(headers = []) ?version ?(status = `Code 200) outchan =
  let resp = Http_response.init ~body:[`String body] ~headers ?version ~status () in
    respond_with resp outchan

let respond_control
    func_name ?(is_valid_status = fun _ -> true) ?(headers=[]) ?(body="")
    ?version status outchan =
  let code = match status with `Code c -> c | #status as s -> code_of_status s in
  if is_valid_status code then
    let headers =
      [ "Content-Type", "text/html; charset=iso-8859-1" ] @ headers
    in
    let body = (control_body code body) ^ body in
      respond ?version ~status ~headers ~body outchan
  else
    failwith
      (sprintf "'%d' isn't a valid status code for %s" code func_name)
      
let respond_redirect ~location ?body ?version ?(status = `Code 301) outchan =
  respond_control "Daemon.respond_redirect" ~is_valid_status:is_redirection
    ~headers:["Location", location] ?body ?version status outchan

let respond_error ?body ?version ?(status = `Code 400) outchan =
  respond_control "Daemon.respond_error" ~is_valid_status:is_error
    ?body ?version status outchan
    
let respond_not_found ~url ?version outchan =
  respond_control "Daemon.respond_not_found" ?version (`Code 404) outchan
    
let respond_forbidden ~url ?version outchan =
  respond_control "Daemon.respond_forbidden" ?version (`Code 403) outchan
    
let respond_unauthorized ?version ?(realm = server_string) outchan =
  let body =
    sprintf "401 - Unauthorized - Authentication failed for realm \"%s\"" realm
  in
    respond ~headers:["WWW-Authenticate", sprintf "Basic realm=\"%s\"" realm]
      ~status:(`Code 401) ~body outchan

let respond_file ~fname ?droot ?(version = default_version) 
    ?(mime_type = "application/octet-stream") outchan =
  (** ASSUMPTION: 'fname' doesn't begin with a "/"; it's relative to the current
      document root (usually the daemon's cwd) *)
  let droot = match droot with
    |None -> Sys.getcwd () |Some d -> d in  (* document root *)
  let path = droot ^ "/" ^ fname in (* full path to the desired file *)
    if not (Sys.file_exists path) then (* file not found *)
      respond_not_found ~url:fname outchan
    else begin
      try
	if Http_misc.is_directory path then (* file found, is a dir *)
          respond_forbidden ~url:fname ~version outchan
	else (* file found, is something else *)
          Lwt_io.with_file ~mode:Lwt_io.input path
            (fun inchan ->
	       lwt file_size = Lwt_io.file_length path in
               let (_, finished) = Lwt.wait () in (* don't care when file is finished *)
	       let resp = Http_response.init ~body:[`Inchan (file_size, inchan, finished)]
		 ~status:(`Code 200) ~version ()
	       in respond_with resp outchan
            )
      with
	| Unix.Unix_error (Unix.EACCES, _, _)
	| Sys_error _ ->
            respond_forbidden ~url:fname ~version outchan
    end
      
(** internal: this exception is raised after a malformed request has been read
    by a serving process to signal main server (or itself if mode = `Single) to
    skip to next request *)
exception Again;;

  (* given a Http_parser.parse_request like function, wrap it in a function that
  do the same and additionally catch parsing exception sending HTTP error
  messages back to client as needed. Returned function raises Again when it
  encounter a parse error (name 'Again' is intended for future versions that
  will support http keep alive signaling that a new request has to be parsed
  from client) *)
let rec wrap_parse_request_w_safety parse_function (inchan:Lwt_io.input_channel) outchan =
  let pp_parse_exc e =
    sprintf "HTTP request parse error: %s" (Printexc.to_string e)
  in catch (fun () -> parse_function inchan)
       (function
	  | (Malformed_request req) as e ->
	      debug_print (pp_parse_exc e);
	      respond_error ~status:(`Code 400)
		~body:("request 1st line format should be: " ^
			 "'&lt;method&gt; &lt;url&gt; &lt;version&gt;'" ^
			 "<br />\nwhile received request 1st line was:<br />\n" ^ req)
		outchan >>
		fail Again
	  | (Invalid_HTTP_method meth) as e ->
	      debug_print (pp_parse_exc e);
	      respond_error ~status:(`Code 501)
		~body:("Method '" ^ meth ^ "' isn't supported (yet)")
		outchan >>
		fail Again
	  | (Malformed_request_URI uri) as e ->
	      debug_print (pp_parse_exc e);
	      respond_error ~status:(`Code 400) ~body:("Malformed URL: '" ^ uri ^ "'")
		outchan >>
		fail Again
	  | (Invalid_HTTP_version version) as e ->
	      debug_print (pp_parse_exc e);
	      respond_error ~status:(`Code 505)
		~body:("HTTP version '" ^ version ^ "' isn't supported (yet)")
		outchan >>
		fail Again
	  | (Malformed_query query) as e ->
	      debug_print (pp_parse_exc e);
	      respond_error ~status:(`Code 400)
		~body:(sprintf "Malformed query string '%s'" query) outchan >>
		fail Again
	  | (Malformed_query_part (binding, query)) as e ->
	      debug_print (pp_parse_exc e);
	      respond_error ~status:(`Code 400)
		~body:(sprintf "Malformed query part '%s' in query '%s'" binding query)
		outchan >>
		fail Again
	  | e -> fail e)
       
  (* TODO what happens when a Quit exception is raised by a callback? Do other
  callbacks keep on living until the end or are they all killed immediately?
  The right semantics should obviously be the first one *)

  (** - handle HTTP authentication
   *  - handle automatic closures of client connections *)
let invoke_callback conn_id (req:Http_request.request) spec (outchan:Lwt_io.output_channel Lwt.t) =
  try_lwt 
    (match (spec.auth, (Http_request.authorization req)) with
       | `None, _ -> spec.callback conn_id req outchan  (* no auth required *)
       | `Basic (realm, authfn), Some (`Basic (username, password)) ->
	   if authfn username password then spec.callback conn_id req outchan (* auth ok *)
	   else fail (Unauthorized realm)
       | `Basic (realm, _), _ -> fail (Unauthorized realm)) (* auth failure *)
 with
   | Unauthorized realm -> respond_unauthorized ~realm outchan
   | Again -> return ()

let daemon_callback spec =
  let conn_id = ref 0 in
  let daemon_callback ~clisockaddr ~srvsockaddr inchan outchan =
    let conn_id = incr conn_id; !conn_id in
    let rec loop prev =
      catch (fun () -> 
        debug_print "request";
        let (finished_t, finished_u) = Lwt.wait () in
        let outchan = prev >> Lwt.return outchan in (* wait for response to finish before writing another *)
        lwt req = wrap_parse_request_w_safety 
          (Http_request.init_request ~clisockaddr ~srvsockaddr finished_u) 
          inchan outchan in
        (* XXX Again exceptions should wakeup finished_u and loop with wakeup'd thread *)
        debug_print "invoke_callback";
        let prev = invoke_callback conn_id req spec outchan in
        lwt () = finished_t in (* wait for request to finish before reading another *)
        loop prev
      ) ( function 
         | End_of_file -> debug_print "done with connection"; spec.conn_closed conn_id; return ()
         | Canceled -> debug_print "cancelled"; spec.conn_closed conn_id; return ()
         | e -> fail e )
    in
    debug_print "server starting";
    try_lwt
      loop (Lwt.return ())
    with
      | exn ->
	  debug_print (sprintf "uncaught exception: %s" (Printexc.to_string exn));
	  spec.exn_handler exn (Lwt.return outchan) (* XXX be sure callbacks are finished *)
  in
  daemon_callback
       
let main spec =
  let () = match spec.root_dir with Some dir -> Sys.chdir dir | None -> () in
  lwt sockaddr = Http_misc.build_sockaddr (spec.address, spec.port) in
  Http_tcp_server.simple ~sockaddr ~timeout:spec.timeout (daemon_callback spec)

module Trivial =
  struct
    let heading_slash_RE = Pcre.regexp "^/"

    let trivial_callback _ req (outchan:Lwt_io.output_channel Lwt.t) =
      debug_print "trivial_callback";
      let path = Http_request.path req in
      if not (Pcre.pmatch ~rex:heading_slash_RE path) then
        respond_error ~status:(`Code 400) outchan
      else
        respond_file ~fname:(Http_misc.strip_heading_slash path) outchan

    let callback = trivial_callback

    let main spec = main { spec with callback = trivial_callback }
  end

let default_callback _ _ _ = return ()
let default_exn_handler exn _ =
  debug_print "no handler given: re-raising";
  fail exn

let default_conn_closed conn_id = ()

let default_spec = {
  address = "0.0.0.0";
  auth = `None;
  auto_close = false;
  callback = default_callback;
  conn_closed = default_conn_closed;
  port = 80;
  root_dir = None;
  exn_handler = default_exn_handler;
  timeout = Some 300;
}

