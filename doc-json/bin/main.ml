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
    description : string option [@yojson.option];
    parent : string;
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
  val make_map : module_list -> (string, module_) Hashtbl.t
end = struct
  let from_yojson json = module_list_of_yojson json

  let make_map l =
    let map = Hashtbl.create (List.length l) in
    List.iter (fun (m: module_) -> Hashtbl.add map m.name m) l;
    map
end

module Methods : sig
  type t
  val from_modules : module_list -> t
  val print_names : t -> unit
  val make_map : t -> (string, method_) Hashtbl.t
end = struct
  type t = method_ list

  let from_modules modules = List.map (fun m ->
    match m.methods with
    | Some x -> x
    | None -> []
  ) modules |> Stdune.List.concat

  let make_map l =
    let map = Hashtbl.create (List.length l) in
    List.iter (fun (m: method_) -> Hashtbl.add map m.name m) l;
    map

  let print_names t = List.iter (fun (m: method_) -> Stdio.print_endline m.name) t
end

module Constants : sig
  type t
  val from_modules : module_list -> t
  val make_map : t -> (string, constant) Hashtbl.t
end = struct
  type t = constant list

  let from_modules modules = List.map (fun m ->
    match m.constants with
    | Some x -> x
    | None -> []
  ) modules |> Stdune.List.concat

  let make_map l =
    let map = Hashtbl.create (List.length l) in
    List.iter (fun (m: constant) -> Hashtbl.add map m.name m) l;
    map
end

let () =
  let f = Core.In_channel.create "./api_docs/monkeyc.json" in
  let yojson_string = Yojson.Safe.from_string (Stdune.Io.read_all f) in
  let modules = Modules.from_yojson yojson_string in
  let map_of_modules = Modules.make_map modules in
  let methods = Methods.from_modules modules in
  let map_of_methods = Methods.make_map methods in
  let constants = Constants.from_modules modules in
  let map_of_constants = Constants.make_map constants in
  print_endline "";
  Methods.print_names methods;
  print_endline ("\nmodules: " ^ (Hashtbl.find map_of_modules "Toybox.Weather").url ^ "\n");
  print_endline ("\nparameters: " ^ (List.hd (Hashtbl.find map_of_methods "getSunrise").ast.parameters).name ^ "\n");
  print_endline ("\nconstants: " ^ (Hashtbl.find map_of_constants "CONDITION_CLEAR").value ^ "\n");
