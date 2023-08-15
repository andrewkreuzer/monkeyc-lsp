open Lexer

let print_token_type = function
  | PUNCTUATION LPAREN -> "LPAREN"
  | PUNCTUATION RPAREN -> "RPAREN"
  | PUNCTUATION LBRACE -> "LBRACE"
  | PUNCTUATION RBRACE -> "RBRACE"
  | PUNCTUATION LBRACKET -> "LBRACKET"
  | PUNCTUATION RBRACKET -> "RBRACKET"
  | PUNCTUATION SEMICOLON -> "SEMICOLON"
  | PUNCTUATION DOT -> "DOT"
  | PUNCTUATION COLON -> "COLON"
  | PUNCTUATION COMMA -> "COMMA"
  | PUNCTUATION QUESTIONMARK -> "QUESTIONMARK"
  | BOOLEAN b -> "BOOLEAN " ^ (string_of_bool b)
  | OPERATOR o -> "OPERATOR " ^ (string_of_operator o)
  | IDENT -> "IDENT "
  | STRING -> "STRING "
  | EOF -> "EOF"
  | NUMBER -> "NUMBER "
  | KEYWORD k -> "KEYWORD " ^ (string_of_keyword k)
  | COMMENT -> "COMMENT"
  | _ -> "ERROR"

let token_string t =
  print_token_type
    t.token_type
  ^ " " ^ t.value
  ^ " " ^ (string_of_int t.line)
  ^ " " ^ (string_of_int t.col)

let rec token_list_string = function
  | [] -> ""
  | t::ts -> token_string t ^ "\n" ^ token_list_string ts

let print_token t = 
    print_endline (token_string t)

let print_token_list l = 
  List.iter (fun ( t : token) ->
    print_token t
  ) l
