let run file =
  let lexer = Lexer.make file in
  let tokens : Lexer.token list = Lexer.tokens lexer in
  match tokens with
    | [] -> print_endline "No tokens"
    | _ -> List.iter (fun ( t : Lexer.token) ->
      print_endline (
      Lexer.print_token
        t.token_type ^ " "
      ^ t.value ^ " "
      ^ (string_of_int t.line) ^ " "
      ^ (string_of_int t.col))
  ) tokens
