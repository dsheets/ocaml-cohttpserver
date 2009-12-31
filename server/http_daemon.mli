open Cohttp

type daemon_spec = {
  address : string;
  auth : Http_types.auth_info;
  callback : Http_request.request -> Lwt_io.output_channel -> unit Lwt.t;
  port : int;
  root_dir : string option;
  exn_handler : exn -> Lwt_io.output_channel -> unit Lwt.t;
  timeout : int option;
  auto_close : bool;
}
val respond :
  ?body:string ->
  ?headers:(string * string) list ->
  ?version:Cohttp.Http_types.version ->
  ?status:Cohttp.Http_types.status_code -> Lwt_io.output_channel -> unit Lwt.t
val respond_control :
  string ->
  ?is_valid_status:(int -> bool) ->
  ?headers:(string * string) list ->
  ?body:string ->
  ?version:Cohttp.Http_types.version ->
  Cohttp.Http_types.status_code -> Lwt_io.output_channel -> unit Lwt.t
val respond_redirect :
  location:string ->
  ?body:string ->
  ?version:Cohttp.Http_types.version ->
  ?status:Cohttp.Http_types.status_code -> Lwt_io.output_channel -> unit Lwt.t
val respond_error :
  ?body:string ->
  ?version:Cohttp.Http_types.version ->
  ?status:Cohttp.Http_types.status_code -> Lwt_io.output_channel -> unit Lwt.t
val respond_not_found :
  url:'a ->
  ?version:Cohttp.Http_types.version -> Lwt_io.output_channel -> unit Lwt.t
val respond_forbidden :
  url:'a ->
  ?version:Cohttp.Http_types.version -> Lwt_io.output_channel -> unit Lwt.t
val respond_unauthorized :
  ?version:'a -> ?realm:string -> Lwt_io.output_channel -> unit Lwt.t
val respond_file :
  fname:Lwt_io.file_name ->
  ?droot:string ->
  ?version:Cohttp.Http_types.version -> ?mime_type: string ->  Lwt_io.output_channel -> unit Lwt.t
val respond_with :
  Cohttp.Http_response.response -> Lwt_io.output_channel -> unit Lwt.t
val main : daemon_spec -> 'a Lwt.t
module Trivial :
  sig
    val heading_slash_RE : Pcre.regexp
    val trivial_callback :
      Cohttp.Http_request.request -> Lwt_io.output_channel -> unit Lwt.t
    val callback :
      Cohttp.Http_request.request -> Lwt_io.output_channel -> unit Lwt.t
    val main : daemon_spec -> 'a Lwt.t
  end
val default_spec : daemon_spec
