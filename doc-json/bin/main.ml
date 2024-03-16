open Stdune
open Core
open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(*
went back to a single typedef for now
the polymorphic type is not working
 *)
type type_ =
  { t : string [@key "type"];
    valueType : type_ list option [@yojson.option];
    keys : parameter list option [@yojson.option];
    parameters : parameter list option [@yojson.option];
    returns : type_ list option [@yojson.option];
  } [@@deriving yojson] [@@yojson.allow_extra_fields]

and parameter =
  { name : string;
    types : type_ list option [@yojson.option];
  } [@@deriving yojson] [@@yojson.allow_extra_fields]

type ast =
  { name : string;
    parameters : parameter list;
    returns : type_ list;
  } [@@deriving yojson] [@@yojson.allow_extra_fields]

type parameter_doc =
  { name : string option [@yojson.option];
    docstring: string option [@yojson.option];
    dictionary_keys : parameter_doc list option [@yojson.option];
  } [@@deriving yojson] [@@yojson.allow_extra_fields]

type method_ =
  { name : string;
    ast : ast;
    parameters : parameter_doc list option;
    nullable : bool;
    void : bool;
    depricated : bool;
    throws : parameter_doc list option;
    returns : string list;
  } [@@deriving yojson] [@@yojson.allow_extra_fields]

type constant =
  { name : string;
    value : string;
    since : string;
    description : string;
    type_ : string [@key "type"];
  } [@@deriving yojson]

type typedef =
  { name : string;
    types : string list;
  } [@@deriving yojson]

type module_ =
  { module_type : string [@key "type"];
    name : string;
    url : string;
    modules : string list option [@yojson.option];
    docstring : string option [@yojson.option];
    classes : string list option [@yojson.option];
    constants : constant list option [@yojson.option];
    typedefs : typedef list option [@yojson.option];
    methods : method_ list option [@yojson.option];
  } [@@deriving yojson] [@@yojson.allow_extra_fields]

type module_list = module_ list [@@deriving yojson]

module Modules : sig
  val from_yojson : Yojson.Safe.t -> module_list
end = struct
  let from_yojson json = module_list_of_yojson json
end

module Methods : sig
  type t
  val from_modules : module_ list -> t
  val print_names : t -> unit
end = struct
  type t = method_ list

  let from_modules modules = List.map modules ~f:(fun m ->
    match m.methods with
    | Some x -> x
    | None -> []
  ) |> List.concat

  (* why the fuck does this require a typedef *)
  let print_names (t: method_ list) = List.iter t ~f:(fun m -> Stdio.print_endline m.name)
end

let () =
  let f = In_channel.create "./api_docs/monkeyc.json" in
  let yojson_string = Yojson.Safe.from_string (Io.read_all f) in
  (* Stdio.print_endline (Yojson.Safe.show yojson_string); *)
  let modules = Modules.from_yojson yojson_string in
  let _methods = Methods.from_modules modules in
  Stdio.print_endline "";
  Methods.print_names _methods;
(* Stdio.print_endline (Stdlib.read_line ()) *)
