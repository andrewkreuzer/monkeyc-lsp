let run file =
  let lexer = Lexer.make file in
  let tokens = Lexer.tokens lexer in
  match tokens with
    | [] -> print_endline "No tokens"
    | _ -> List.iter (fun t -> print_endline (Lexer.print_token t)) tokens
