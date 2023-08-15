let run file =
  let lexer = Lexer.make file in
  let tokens : Lexer.token list = Lexer.tokens lexer in
  match tokens with
  | [] -> print_endline "No tokens"
  | _ -> Pprint.print_token_list tokens

exception ParserError of string

type access_modifier =
  | PRIVATE
  | PUBLIC

type statement =
  | USING of using
  | EXPRESSION of expression
  | DEFINITION of definition
  | IF of if_statement

and definition =
  | CLASS
  | FUNCTION of function_definition

and using = { namespace: string }

and if_statement =
  { condition: string
    ; if_body: statement list
  }
and block = statement list

and function_definition =
  { func_name: string
    ; parameters : parameter list
    ; has_return_type: bool
    ; return_type: string
    ; access_modifier: access_modifier
    ; func_body: block
  }

and expression =
  | IDENT of string
  | ENUM of enum
  | FUNCTION_CALL of function_call
  | NEW of new_expression
  | ASSIGNMENT of assignment
  | ADD of expression * expression
  | SUBTRACT of expression * expression
  | VARIABLE of expression

and new_expression = { expression: expression }

and assignment =
  { left: expression
    ; operator: string
    ; right: expression
  }

and function_call =
  { object_name: string
    ; function_name: string
    ; func_parameters: parameter list
  }

and parameter =
  { par_name: string
    ; par_has_type: bool
    ; par_type_name: string
    ; nullable: bool
  }

and var =
  { var_name: string
    ; var_has_type: bool
    ; var_type_name: string
    ; value: string
    ; var_access_modifier: access_modifier
  }

and enum =
  { enum_name: string
    ; enum_values: string list
  }

type class_definition =
  { class_name: string
    ; extends: bool
    ; extends_class: Lexer.token list
    ; private_vars: var list
    ; public_vars: var list
    ; private_functions: function_definition list
    ; public_functions: function_definition list
  }

let read_var stream access_modifier =
  let is_first i = i.var_name = "" in
  let rec aux st ret =
    let t, s = Lexer.next_token st in
    match t.token_type with
    | Lexer.IDENT when (is_first ret) -> aux s { ret with var_name = t.value }
    | Lexer.KEYWORD AS -> aux s { ret with var_has_type = true }
    | Lexer.IDENT when ret.var_has_type = true -> aux s { ret with var_type_name = t.value }
    | Lexer.PUNCTUATION SEMICOLON -> ret, s
    | _ -> raise (ParserError ("Unexpected token in private var expression " ^ Pprint.token_string t))
  in
  aux stream { var_name = ""; var_has_type = true; var_type_name = ""; value = ""; var_access_modifier = access_modifier }

let read_parameter stream =
  let is_first (i: parameter) = i.par_name = "" in
  let rec aux st (ret: parameter) =
    let t, s = Lexer.next_token st in
    match t.token_type with
    | Lexer.IDENT when (is_first ret) -> aux s { ret with par_name = t.value }
    | Lexer.IDENT when ret.par_has_type = true -> aux s { ret with par_type_name = t.value }
    | Lexer.KEYWORD AS -> aux s { ret with par_has_type = true }
    | Lexer.PUNCTUATION QUESTIONMARK -> aux s { ret with nullable = true }
    | Lexer.PUNCTUATION COMMA -> ret, st (* send back token stream before comma *)
    | Lexer.PUNCTUATION RPAREN -> ret, st (* send back token stream before paren *)
    | _ -> raise (ParserError ("Unexpected token in parameter " ^ Pprint.token_string t))
  in
  aux stream { par_name = ""; par_has_type = true; par_type_name = ""; nullable = false }

let read_parameters stream =
  let rec aux st ret =
    let t, s = Lexer.next_token st in
    match t.token_type with
    | Lexer.IDENT ->
      let parameter, s = read_parameter st in
      aux s (parameter :: ret)
    | Lexer.PUNCTUATION COMMA -> aux s ret (* read_parameter finished *)
    | Lexer.PUNCTUATION RPAREN -> List.rev ret, s
    | _ -> raise (ParserError ("Unexpected token in parameter list " ^ Pprint.token_string t))
  in
  aux stream []

let read_condition stream =
  let rec aux st ret =
    let t, s = Lexer.next_token st in
    match t.token_type with
    | Lexer.PUNCTUATION RPAREN -> ret, s
    | _ -> aux s (t.value :: ret)
  in
  let seq, s = aux stream [] in
  String.concat " " (List.rev seq), s

let rec read_block stream =
  let rec aux st ret =
    let t, s = Lexer.next_token st in
    match t.token_type with
    | Lexer.IDENT ->
      let exp, s = read_expression st in
      aux s (EXPRESSION exp :: ret)
    | Lexer.KEYWORD IF ->
      let if_statement, s = read_if_statement s in
      aux s (if_statement :: ret)
    | Lexer.PUNCTUATION SEMICOLON -> aux s ret
    | Lexer.PUNCTUATION RBRACE -> List.rev ret, st
    | _ -> raise (ParserError ("Unexpected token in block " ^ Pprint.token_string t))
  in
  aux stream []

and read_if_statement stream =
  let rec aux st if_def =
    let t, s = Lexer.next_token st in
    match t.token_type with
    | Lexer.PUNCTUATION LPAREN ->
      let condition, s = read_condition s in
      aux s { if_def with condition = condition }
    | Lexer.PUNCTUATION LBRACE ->
      let body, s = read_block s in
      aux s { if_def with if_body = body }
    | Lexer.PUNCTUATION RBRACE -> IF if_def, st
    | _ -> raise (ParserError ("Unexpected token in if statement " ^ Pprint.token_string t))
  in
  aux stream { condition = ""; if_body = [] }

and read_expression stream =
  let rec aux str ret =
    let t, st = Lexer.next_token str in
    match ret with
    | [] -> aux st [(t.token_type, t)]
    | (Lexer.KEYWORD NEW, _) :: _ ->
      let exp, _ = read_expression str in
      NEW { expression = exp }, st
    | (Lexer.IDENT, object_name) ::
      (Lexer.OPERATOR EQ, operator) ::
      _ ->
      let exp, _ = read_expression str in
      ASSIGNMENT { left = VARIABLE (IDENT object_name.value); operator = operator.value; right = exp }, st
    | (Lexer.IDENT, function_name)
      :: (Lexer.PUNCTUATION LPAREN, _)
      :: _ ->
      let parameters, _ = read_parameters str in
      FUNCTION_CALL { object_name = ""; function_name = function_name.value; func_parameters = parameters }, st
    | (Lexer.IDENT, object_name)
      :: (Lexer.PUNCTUATION DOT, _)
      :: (Lexer.IDENT, function_name)
      :: (Lexer.PUNCTUATION LPAREN, _)
      :: _ ->
      let parameters, _ = read_parameters str in
      FUNCTION_CALL { object_name = object_name.value; function_name = function_name.value; func_parameters = parameters }, st
    | _ -> aux st (List.append ret [(t.token_type, t)])
  in
  aux stream []

let read_function stream access_modifier =
  let name, st = Lexer.next_token stream in (* assume this is the name *)
  let func_def = { func_name = name.value; parameters = []; has_return_type = false; return_type = ""; access_modifier = access_modifier; func_body = []} in
  let rec aux st func_def =
    let t, s = Lexer.next_token st in
    match t.token_type with
    | Lexer.PUNCTUATION LPAREN ->
      let parameters, s = read_parameters s in
      aux s { func_def with parameters = parameters }
    | Lexer.KEYWORD AS -> aux s { func_def with has_return_type = true }
    | Lexer.IDENT when func_def.has_return_type = true -> aux s { func_def with return_type = t.value }
    | Lexer.PUNCTUATION LBRACE ->
      let body, s = read_block s in
      aux s { func_def with func_body = body }
    | Lexer.PUNCTUATION RBRACE -> func_def, s
    | _ -> raise (ParserError ("Unexpected token in function definition " ^ Pprint.token_string t))
  in
  aux st func_def

let read_class_definition stream class_def =
  let is_first i = i.class_name = "" in
  let rec aux st ret =
    let t, s = Lexer.next_token st in
    match t.token_type with
    | Lexer.IDENT when (is_first ret) -> aux s { ret with class_name = t.value }
    | Lexer.KEYWORD EXTENDS -> aux s { ret with extends = true }
    | Lexer.PUNCTUATION DOT -> aux s { ret with extends_class = t :: ret.extends_class }
    | Lexer.IDENT when (ret.extends = true) -> aux s { ret with extends_class = t :: ret.extends_class }
    | Lexer.PUNCTUATION LBRACE -> { ret with extends_class = List.rev ret.extends_class }, s
    | _ -> raise (ParserError ("Unexpected token in class definition " ^ Pprint.token_string t))
  in
  aux stream class_def

let read_class_block stream class_def = 
  let rec aux str ret =
    let t, st = Lexer.next_token str in
    match t.token_type with

    | Lexer.KEYWORD VAR ->
      let var, s = read_var st PUBLIC in
      aux s { ret with private_vars = var :: ret.private_vars }

    | Lexer.KEYWORD PRIVATE -> 
      (
        let t, st = Lexer.next_token st in
        match t.token_type with
        | Lexer.KEYWORD VAR ->
          let var, s = read_var st PRIVATE in
          aux s { ret with private_vars = var :: ret.private_vars }
        | Lexer.KEYWORD FUNCTION ->
          let f, s = read_function st PRIVATE in
          aux s { ret with private_functions = f :: ret.private_functions }
        | _ -> raise (ParserError ("Unexpected token in private declaration " ^ Pprint.token_string t))
      )

    | Lexer.KEYWORD PUBLIC -> 
      (
        let t, st = Lexer.next_token st in
        match t.token_type with
        | Lexer.KEYWORD VAR ->
          let private_var, s = read_var st PUBLIC in
          aux s { ret with public_vars = private_var :: ret.public_vars }
        | Lexer.KEYWORD FUNCTION ->
          let f, s = read_function st PUBLIC in
          aux s { ret with public_functions = f :: ret.public_functions }
        | _ -> raise (ParserError ("Unexpected token in private declaration " ^ Pprint.token_string t))
      )

    | Lexer.PUNCTUATION LBRACE -> { ret with extends_class = List.rev ret.extends_class; private_vars = List.rev ret.private_vars }, st
    | Lexer.PUNCTUATION RBRACE -> ret, st
    | _ -> raise (ParserError ("Unexpected token in class block " ^ Pprint.token_string t))
  in
  aux stream class_def

let read_class stream =
  let class_def = { class_name = ""; extends = false; extends_class = []; public_vars = []; private_vars = []; public_functions = []; private_functions = []} in
  let class_def, s = read_class_definition stream class_def in
  let class_def, s = read_class_block s class_def in
  class_def, s

let read_using_statement stream =
  let rec aux st using =
    let t, s = Lexer.next_token st in
    match t.token_type with
    | Lexer.IDENT -> aux s { namespace = using.namespace ^ t.value }
    | Lexer.PUNCTUATION DOT -> aux s { namespace = using.namespace ^ t.value }
    | Lexer.PUNCTUATION SEMICOLON -> USING using, s
    | _ -> raise (ParserError ("Unexpected token in using expression " ^ Pprint.token_string t))
  in
  aux stream { namespace = "" }

let read_enum_expression stream = 
  let is_first enum = enum.enum_name = "" in
  let rec aux st enum =
    let t, s = Lexer.next_token st in
    match t.token_type with
    | Lexer.IDENT when (is_first enum) -> aux s { enum with enum_name = t.value }
    | Lexer.PUNCTUATION LBRACE -> aux s enum
    | Lexer.IDENT -> aux s { enum with enum_values = t.value :: enum.enum_values }
    | Lexer.PUNCTUATION COMMA -> aux s enum
    | Lexer.PUNCTUATION RBRACE -> ENUM { enum with enum_values = List.rev enum.enum_values }, s
    | _ -> raise (ParserError ("Unexpected token in enum expression " ^ Pprint.token_string t))
  in
  aux stream { enum_name = ""; enum_values = [] }

let var_list_string (vars: var list) =
  let rec aux ret = function
    | [] -> ret
    | h :: t -> aux (ret ^ " " ^ h.var_type_name ^ " " ^ h.var_name) t
  in
  aux "" vars

let rec exp_string = function
  | IDENT s -> s
  | ENUM e -> e.enum_name
  | FUNCTION_CALL f -> f.function_name
  | NEW n -> "New " ^ exp_string n.expression
  | ASSIGNMENT a -> exp_string a.left ^ " = " ^ exp_string a.right
  | ADD (a, b) -> exp_string a ^ " + " ^ exp_string b
  | SUBTRACT (a, b) -> exp_string a ^ " - " ^ exp_string b
  | VARIABLE v -> exp_string v

let exp_list_string (exps: expression list) =
  let rec aux ret = function
    | [] -> ret
    | h :: t ->
      aux (ret ^ " " ^ (exp_string h)) t
  in
  aux "" exps

let statement_string = function
  | EXPRESSION exp -> exp_string exp
  | _ -> "Unknown"

let statement_list_string statements =
  let rec aux ret = function
    | [] -> ret
    | h :: t -> aux (ret ^ " " ^ (statement_string h)) t
  in
  aux "" statements

let func_body_string (body: statement list) =
  let rec aux ret = function
    | [] -> ret
    | h :: t -> aux (ret ^ " " ^ (statement_string h)) t
  in
  aux "" body

let func_list_string (funcs: function_definition list) =
  let rec aux ret = function
    | [] -> ret
    | h :: t -> aux (ret ^ " " ^ h.func_name ^ "\n" ^ (func_body_string h.func_body)) t
  in
  aux "" funcs

let print_class_definitiion class_def =
  print_endline (
    "CLASS " ^ class_def.class_name
    ^ " EXTENDS " ^ (Pprint.token_list_string class_def.extends_class)
    ^ "\nBODY PRIV VARS" ^ (var_list_string class_def.private_vars)
    ^ "\nBODY PUB VARS" ^ (var_list_string class_def.public_vars)
    ^ "\nBODY PRIV FUNC" ^ (func_list_string class_def.private_functions)
    ^ "\nBODY PUB FUNC" ^ (func_list_string class_def.public_functions)
  )

let print = function 
  | USING u -> print_endline ("USING " ^ u.namespace)
  | EXPRESSION ENUM e -> print_endline ("ENUM " ^ e.enum_name ^ " with " ^ String.concat " " e.enum_values)
  | _ -> print_endline "Undefined_recursive_module"

let run_next file =
  let stream = Lexer.make file in
  let rec aux str =
    let t, st = Lexer.next_token str in
    match t.token_type with
    | Lexer.EOF -> print_endline "EOF"
    | Lexer.KEYWORD USING -> 
      let use_exp, s = read_using_statement st in
      print use_exp; aux s
    | Lexer.KEYWORD ENUM -> 
      let enum, s = read_enum_expression st in
      print (EXPRESSION enum); aux s
    | Lexer.KEYWORD CLASS -> 
      let class_def, s = read_class st in
      print_class_definitiion class_def; aux s
    | _ -> print_endline "Undefined_recursive_module"
  in
  aux stream
