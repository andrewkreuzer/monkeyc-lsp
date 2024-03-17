open Monkeyc_docs.Docs

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
  Modules.print_names modules;
  Methods.print_names methods;
  print_endline ("\nmodules: " ^ (Hashtbl.find map_of_modules "Toybox.Weather").url ^ "\n");
  print_endline ("\nparameters: " ^ (List.hd (Hashtbl.find map_of_methods "getSunrise").ast.parameters).name ^ "\n");
  print_endline ("\nconstants: " ^ (Hashtbl.find map_of_constants "CONDITION_CLEAR").value ^ "\n");
