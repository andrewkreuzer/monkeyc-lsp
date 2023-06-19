open Monkeyc_parser

let lexer = Lexer.make "monkeyc-lsp-parser/lib/dune";;


let count = ref 0 in
while !count <= 10 do
  count := !count + 1;

  match Lexer.read_next lexer with
  | EOF -> print_endline "EOF"
  | PUNCTUATION(LBRACKET (c)) -> print_endline ("LBRACKET value: " ^ c)
  | _ -> print_endline "Not EOF"
done;;
