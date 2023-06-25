open Monkeyc_parser

let input_file = ref ""

let usage_msg = "parse <file1>"
let anon_fun filename = input_file := filename
let speclist = []

let () =
  Arg.parse speclist anon_fun usage_msg;
  match !input_file with
  |  "" -> Arg.usage speclist usage_msg
  | f -> Parser.run f
