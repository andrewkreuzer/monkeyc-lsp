open Lexer

let run file =
  let lexer = make file in
  let tokens : token list = tokens lexer in
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
  | CLASS of class_definition
  | FUNCTION of function_definition

and using = { namespace: namespace }
and namespace = string list

and if_statement =
  { condition: expression
    ; if_body: statement list
    ; if_return_exp: expression option
    ; if_return_type: type_definition option
    ; else_body: statement list option list
  }

and block = statement list

and function_definition =
  { func_name: string
    ; parameters : parameter list
    ; return_type: type_definition option
    ; access_modifier: access_modifier
    ; func_body: block
    ; func_return_exp: expression option
    ; func_return_type: type_definition option
  }

and expression =
  | IDENT of string
  | BINARY of binary
  | ENUM of enum
  | FUNCTION_CALL of function_call
  | NEW of new_expression
  | ADD of expression * expression
  | SUBTRACT of expression * expression
  | VARIABLE of var
  | ARRAY of expression list
  | RETURN of expression option

and type_definition =
  { type_name: namespace
    ; type_nullable: bool
    ; contained_type: type_definition option
    ; type_is_array: bool
    ; type_array: type_definition list
  }

and new_expression = { expression: expression }

and binary = 
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
  { param_name: string
    ; param_type: type_definition option
  }

and var =
  { var_name: string
    ; value: expression option
    ; var_type: type_definition option
    ; var_access_modifier: access_modifier
  }

and enum =
  { enum_name: string
    ; enum_values: string list
  }

and class_definition =
  { class_name: string
    ; extends: bool
    ; extends_class: namespace
    ; private_vars: var list
    ; properties: var list
    ; private_functions: function_definition list
    ; methods: function_definition list
  }

let rec read_type stream read_until =
  let rec aux st ret =
    let t, s = next_token st in
    match t.token_type with
    | IDENT -> aux s { ret with type_name = t.value :: ret.type_name }
    | PUNCTUATION DOT -> aux s ret
    | OPERATOR LT ->
      let contained_type, s = read_type s (OPERATOR GT) in
      aux s { ret with contained_type = Some contained_type }
    | KEYWORD OR ->
      let multi_type, s = read_type s read_until in
      aux s { ret with type_is_array = true; type_array = multi_type :: ret.type_array }
    | PUNCTUATION QUESTIONMARK -> aux s { ret with type_nullable = true }
    | tt when tt = read_until -> ret, st
    | OPERATOR GT -> aux s ret
    | PUNCTUATION COMMA -> ret, st (* hit end of type definition in parameters *)
    | _ -> raise (ParserError ("Unexpected token in type " ^ Pprint.token_string t))
  in
  aux stream { type_name = []; type_nullable = false; contained_type = None; type_is_array = false; type_array = [] }

let read_parameter stream =
  let is_first (i: parameter) = i.param_name = "" in
  let rec aux st (ret: parameter) =
    let t, s = next_token st in
    match t.token_type with
    | IDENT when (is_first ret) -> aux s { ret with param_name = t.value }
    | KEYWORD AS ->
      let type_definition, s = read_type s (PUNCTUATION RPAREN) in
      aux s { ret with param_type = Some type_definition }
    | PUNCTUATION COMMA -> ret, st (* send back token stream before comma *)
    | PUNCTUATION RPAREN -> ret, st (* send back token stream before paren *)
    | _ -> raise (ParserError ("Unexpected token in parameter " ^ Pprint.token_string t))
  in
  aux stream { param_name = ""; param_type = None }

let read_parameters stream =
  let rec aux st ret =
    let t, s = next_token st in
    match t.token_type with
    | IDENT ->
      let parameter, s = read_parameter st in
      aux s (parameter :: ret)
    | PUNCTUATION COMMA -> aux s ret (* read_parameter finished *)
    | PUNCTUATION RPAREN -> List.rev ret, s
    | _ -> raise (ParserError ("Unexpected token in parameter list " ^ Pprint.token_string t))
  in
  aux stream []

let rec read_block stream =
  let rec aux st ret =
    let t, s = next_token st in
    match t.token_type with
    | IDENT ->
      let exp, s = read_expression st (PUNCTUATION SEMICOLON) in
      aux s (EXPRESSION exp :: ret)
    | KEYWORD IF ->
      let if_statement, s = read_if_statement s in
      aux s (if_statement :: ret)
    | KEYWORD VAR ->
      let var, s = read_var s PRIVATE in
      aux s ((EXPRESSION (VARIABLE var )):: ret)
    | PUNCTUATION SEMICOLON -> aux s ret
    | KEYWORD RETURN -> (
      let return_exp, s = read_expression s (PUNCTUATION SEMICOLON) in
      let t, s = next_token s in
      match t.token_type with
      | KEYWORD AS ->
        let return_type, s = read_type s (PUNCTUATION SEMICOLON) in
        let t, s = next_token s in
        begin
          match t.token_type with
          | PUNCTUATION SEMICOLON -> List.rev ret, Some return_exp, Some return_type, s
          | _ -> raise (ParserError ("Unexpected token in return statement " ^ Pprint.token_string t))
          end
      | PUNCTUATION SEMICOLON -> List.rev ret, Some return_exp, None, s
      | _ -> raise (ParserError ("Unexpected token in return statement " ^ Pprint.token_string t))
    )
    | PUNCTUATION RBRACE -> List.rev ret, None, None, st
    | _ -> raise (ParserError ("Unexpected token in block " ^ Pprint.token_string t))
  in
  aux stream []

and read_if_statement stream =
  let rec aux st if_def =
    let t, s = next_token st in
    match t.token_type with
    | PUNCTUATION LPAREN ->
      let condition, s = read_expression s (PUNCTUATION RPAREN) in
      aux s { if_def with condition = condition }
    | PUNCTUATION LBRACE ->
      let body, return_exp, return_type, s = read_block s in
      aux s { if_def with if_body = body; if_return_exp = return_exp; if_return_type = return_type }
    | PUNCTUATION RBRACE -> IF if_def, s
    | _ -> raise (ParserError ("Unexpected token in if statement " ^ Pprint.token_string t))
  in
  aux stream { condition = IDENT ""; if_body = []; else_body = [None]; if_return_exp = None; if_return_type = None } (* filling condition with blank ident for now I guess *)

and read_expression stream read_until =
  let rec aux str ret =
    let t, st = next_token str in
    match ret with
    | [] -> aux st [(t.token_type, t)]
    | (KEYWORD NEW, _) :: _ ->
      let exp, s = read_expression str read_until in
      NEW { expression = exp }, s
    | (NUMBER, left) ::
      (OPERATOR LT, operator) ::
      (NUMBER, right) ::
      _ ->
      BINARY { left = IDENT left.value; operator = operator.value; right = IDENT right.value }, st
    | (IDENT, object_name) ::
      (OPERATOR EQ, operator) ::
      _ ->
      let exp, s = read_expression str read_until in
      BINARY { left = IDENT object_name.value; operator = operator.value; right = exp }, s
    | (IDENT, function_name)
      :: (PUNCTUATION LPAREN, _)
      :: _ ->
      let parameters, s = read_parameters str in
      FUNCTION_CALL { object_name = ""; function_name = function_name.value; func_parameters = parameters }, s
    | (IDENT, object_name)
      :: (PUNCTUATION DOT, _)
      :: (IDENT, function_name)
      :: (PUNCTUATION LPAREN, _)
      :: _ ->
      let parameters, s = read_parameters str in
      FUNCTION_CALL { object_name = object_name.value; function_name = function_name.value; func_parameters = parameters }, s
    | (KEYWORD RETURN, _) :: _ ->
        let exp, s = read_expression str read_until in
        RETURN (Some exp), s
    | (PUNCTUATION LBRACKET, _) :: _ ->
      let array, s = read_array str in
      ARRAY array, s
    | (IDENT, _) :: ( tt, _) :: _ when (read_until = tt) -> IDENT t.value, st
    | (IDENT, _) :: ( PUNCTUATION RBRACKET, _) :: _ -> IDENT t.value, str
    | (tt, _) :: _ when tt = read_until -> raise (ParserError ("Unexpected token in expression " ^ Pprint.token_string t))
    | _ -> aux st (List.append ret [(t.token_type, t)])
  in
  aux stream []

and read_var stream access_modifier =
  let is_first i = i.var_name = "" in
  let rec aux st ret =
    let t, s = next_token st in
    match t.token_type with
    | IDENT when (is_first ret) -> aux s { ret with var_name = t.value }
    | KEYWORD AS ->
      let type_definition, s = read_type s (PUNCTUATION SEMICOLON) in
      aux s { ret with var_type = Some type_definition}
    | OPERATOR EQ ->
      let value, s = read_expression s (PUNCTUATION SEMICOLON) in
      aux s { ret with value = Some value}
    | PUNCTUATION SEMICOLON -> ret, s
    | _ -> raise (ParserError ("Unexpected token in var expression " ^ Pprint.token_string t))
  in
  aux stream { var_name = ""; var_type = None; value = None; var_access_modifier = access_modifier }

and read_array stream =
  let rec aux st ret =
    let t, s = next_token st in
    match t.token_type with
    | PUNCTUATION RBRACKET -> List.rev ret, s
    | PUNCTUATION COMMA -> aux s ret
    | _ ->
      let exp, s = read_expression st (PUNCTUATION COMMA) in
      aux s (exp :: ret)
  in
  aux stream []

let read_function stream access_modifier =
  let name, st = next_token stream in (* assume this is the name *)
  let func_def = { func_name = name.value; parameters = []; return_type = None; access_modifier = access_modifier; func_body = []; func_return_exp = None; func_return_type = None } in
  let rec aux st func_def =
    let t, s = next_token st in
    match t.token_type with
    | PUNCTUATION LPAREN ->
      let parameters, s = read_parameters s in
      aux s { func_def with parameters = parameters }
    | KEYWORD AS ->
      let return_type, s = read_type s (PUNCTUATION LBRACE) in
      aux s { func_def with return_type = Some return_type }
    | PUNCTUATION LBRACE ->
      let body, return_exp, return_type, s = read_block s in
      aux s { func_def with func_body = body; func_return_exp = return_exp; func_return_type = return_type }
    | PUNCTUATION RBRACE -> func_def, s
    | _ -> raise (ParserError ("Unexpected token in function definition " ^ Pprint.token_string t))
  in
  aux st func_def

let read_class_definition stream class_def =
  let is_first i = i.class_name = "" in
  let rec aux st ret =
    let t, s = next_token st in
    match t.token_type with
    | IDENT when (is_first ret) -> aux s { ret with class_name = t.value }
    | KEYWORD EXTENDS -> aux s { ret with extends = true }
    | PUNCTUATION DOT -> aux s ret
    | IDENT when (ret.extends = true) -> aux s { ret with extends_class = t.value :: ret.extends_class }
    | PUNCTUATION LBRACE -> { ret with extends_class = List.rev ret.extends_class }, s
    | _ -> raise (ParserError ("Unexpected token in class definition " ^ Pprint.token_string t))
  in
  aux stream class_def

let read_class_block stream class_def = 
  let rec aux str ret =
    let t, st = next_token str in
    match t.token_type with

    | KEYWORD VAR ->
      let var, s = read_var st PUBLIC in
      aux s { ret with private_vars = var :: ret.private_vars }

    | KEYWORD PRIVATE -> 
      (
        let t, st = next_token st in
        match t.token_type with
        | KEYWORD VAR ->
          let var, s = read_var st PRIVATE in
          aux s { ret with private_vars = var :: ret.private_vars }
        | KEYWORD FUNCTION ->
          let f, s = read_function st PRIVATE in
          aux s { ret with private_functions = f :: ret.private_functions }
        | _ -> raise (ParserError ("Unexpected token in private declaration " ^ Pprint.token_string t))
      )

    | KEYWORD PUBLIC -> 
      (
        let t, st = next_token st in
        match t.token_type with
        | KEYWORD VAR ->
          let var, s = read_var st PUBLIC in
          aux s { ret with properties = var :: ret.properties }
        | KEYWORD FUNCTION ->
          let f, s = read_function st PUBLIC in
          aux s { ret with methods = f :: ret.methods }
        | _ -> raise (ParserError ("Unexpected token in private declaration " ^ Pprint.token_string t))
      )

    (* | PUNCTUATION LBRACE -> { ret with extends_class = List.rev ret.extends_class; private_vars = List.rev ret.private_vars }, st *)
    | PUNCTUATION RBRACE -> ret, st
    | _ -> raise (ParserError ("Unexpected token in class block " ^ Pprint.token_string t))
  in
  aux stream class_def

let read_class stream =
  let class_def = { class_name = ""; extends = false; extends_class = []; properties = []; private_vars = []; methods = []; private_functions = []} in
  let class_def, s = read_class_definition stream class_def in
  let class_def, s = read_class_block s class_def in
  class_def, s

let read_using_statement stream =
  let rec aux st using =
    let t, s = next_token st in
    match t.token_type with
    | IDENT -> aux s { namespace = t.value :: using.namespace}
    | PUNCTUATION DOT -> aux s using
    | PUNCTUATION SEMICOLON -> USING using, s
    | _ -> raise (ParserError ("Unexpected token in using expression " ^ Pprint.token_string t))
  in
  aux stream { namespace = [] }

let read_enum_expression stream = 
  let is_first enum = enum.enum_name = "" in
  let rec aux st enum =
    let t, s = next_token st in
    match t.token_type with
    | IDENT when (is_first enum) -> aux s { enum with enum_name = t.value }
    | PUNCTUATION LBRACE -> aux s enum
    | IDENT -> aux s { enum with enum_values = t.value :: enum.enum_values }
    | PUNCTUATION COMMA -> aux s enum
    | PUNCTUATION RBRACE -> ENUM { enum with enum_values = List.rev enum.enum_values }, s
    | _ -> raise (ParserError ("Unexpected token in enum expression " ^ Pprint.token_string t))
  in
  aux stream { enum_name = ""; enum_values = [] }

let var_string var =
  match var.var_type with
  | Some var_type -> String.concat " " var_type.type_name ^ " " ^ var.var_name
  | None -> var.var_name

let var_list_string (vars: var list) =
  let rec aux ret = function
    | [] -> ret
    | h :: t ->
      match h.var_type with
      | Some var_type -> aux (ret ^ " " ^ String.concat " " var_type.type_name ^ " " ^ h.var_name) t
      | None -> aux (ret ^ " " ^ h.var_name) t
  in
  aux "" vars


let rec exp_list_string (exps: expression list) =
  let rec aux ret = function
    | [] -> ret
    | h :: t ->
      aux (ret ^ " " ^ (exp_string h)) t
  in
  aux "" exps

and exp_string = function
  | IDENT s -> s
  | ENUM e -> e.enum_name
  | FUNCTION_CALL f -> f.function_name
  | NEW n -> "New " ^ exp_string n.expression
  | BINARY a -> exp_string a.left ^ " = " ^ exp_string a.right
  | ADD (a, b) -> exp_string a ^ " + " ^ exp_string b
  | SUBTRACT (a, b) -> exp_string a ^ " - " ^ exp_string b
  | VARIABLE v -> var_string v
  | ARRAY a -> "Array " ^ exp_list_string a
  | RETURN r ->
    match r with
    | None -> "Return"
    | Some r -> 
      "Return " ^ exp_string r

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

let print = function 
  | USING u -> print_endline ("USING " ^ String.concat " " u.namespace)
  | EXPRESSION ENUM e -> print_endline ("ENUM " ^ e.enum_name ^ " with " ^ String.concat " " e.enum_values)
  | _ -> print_endline "Undefined_recursive_module"

open Yojson.Basic

let string_of_opt_return_type = function
  | Some t -> String.concat " " t.type_name
  | None -> "void"

let var_to_json (v: var) =
  (`Assoc [
    ("var", `Assoc [
      ("name", `String v.var_name);
      ("type", `String (string_of_opt_return_type v.var_type));
    ])
])

let param_to_json (p: parameter) =
  (`Assoc [
    ("parameter", `Assoc [
      ("name", `String p.param_name);
      ("type", `String (string_of_opt_return_type p.param_type));
    ])
])

let func_to_json (f: function_definition) =
  (`Assoc [
    ("function", `Assoc [
      ("name", `String f.func_name);
      ("return_type", `String (string_of_opt_return_type f.return_type));
      ("parameters", `List (List.map (fun p -> param_to_json p ) f.parameters));
      ("body", `List (List.map (fun s -> `String (statement_string s)) f.func_body));
    ])
])

let class_to_json (c: class_definition) =
  (`Assoc [
    ("class", `Assoc [
      ("name", `String c.class_name);
      ("methods", `List (List.map (fun f ->  func_to_json f) c.methods));
      ("properties", `List (List.map (fun v -> var_to_json v ) c.properties));
      ("private vars", `List (List.map (fun v -> var_to_json v) c.private_vars));
      ("private functions", `List (List.map (fun f -> func_to_json f) c.private_functions));
    ])
  ])

let to_json = function
  | USING u -> pretty_to_string
    (`Assoc [
      ("using", `List (List.map (fun s -> `String s) u.namespace))
    ])
  | EXPRESSION ENUM e -> pretty_to_string 
    (`Assoc [
      ("enum", `Assoc [("name", `String e.enum_name);
      ("values", `List (List.map (fun s -> `String s) e.enum_values))])
    ])
  | DEFINITION CLASS c -> pretty_to_string (class_to_json c)
  | DEFINITION FUNCTION f -> pretty_to_string (func_to_json f)
  | _ -> to_string (`Assoc [("error", `String "Undefined_recursive_module")])

let run_next file =
  let stream = make file in
  let rec aux str =
    let t, st = next_token str in
    match t.token_type with
    | EOF -> print_endline "EOF"
    | KEYWORD USING -> 
      let use_exp, s = read_using_statement st in
      print_endline (to_json use_exp); aux s
    | KEYWORD ENUM -> 
      let enum, s = read_enum_expression st in
      print_endline (to_json (EXPRESSION enum)); aux s
    | KEYWORD CLASS -> 
      let class_def, s = read_class st in
      print_endline (to_json (DEFINITION (CLASS class_def))); aux s
    | _ -> print_endline "Undefined_recursive_module"
  in
  aux stream
