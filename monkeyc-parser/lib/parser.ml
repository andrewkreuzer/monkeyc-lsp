let run file =
  let lexer = Lexer.make file in
  let tokens : Lexer.token list = Lexer.tokens lexer in
  match tokens with
  | [] -> print_endline "No tokens"
  | _ -> Lexer.print_token_list tokens

exception ParserError of string

type expression =
  | USING

type definition =
  | CLASS

type statement =
  | ENUM
  | EXPRESSION of expression
  | DEFINITION of definition

type access_modifier =
  | PRIVATE
  | PUBLIC

type var =
  { name: string
    ; has_type: bool
    ; type_name: string
    ; value: string
    ; access_modifier: access_modifier
  }

let read_var stream access_modifier =
  let is_first i = i.name = "" in
  let rec aux st ret =
  let t, s = Lexer.next_token st in
    match t.token_type with
    | Lexer.KEYWORD VAR -> aux s ret
    | Lexer.IDENT when (is_first ret) -> aux s { ret with name = t.value }
    | Lexer.KEYWORD AS -> aux s { ret with has_type = true }
    | Lexer.IDENT -> aux s { ret with type_name = t.value }
    | Lexer.PUNCTUATION SEMICOLON -> ret, s
    | _ -> raise (ParserError ("Unexpected token in private var expression " ^ Lexer.token_string t))
in
  aux stream { name = ""; has_type = true; type_name = ""; value = ""; access_modifier = access_modifier }

type parameter =
  { name: string
    ; has_type: bool
    ; type_name: string
  }

let read_parameter stream =
  let is_first i = i.name = "" in
  let rec aux st ret =
    let t, s = Lexer.next_token st in
    match t.token_type with
    | Lexer.IDENT when (is_first ret) -> aux s { ret with name = t.value }
    | Lexer.IDENT when ret.has_type = true -> aux s { ret with type_name = t.value }
    | Lexer.KEYWORD AS -> aux s { ret with has_type = true }
    | Lexer.PUNCTUATION COMMA -> ret, st (* send back token stream before comma *)
    | Lexer.PUNCTUATION RPAREN -> ret, st (* send back token stream before comma *)
    | _ -> raise (ParserError ("Unexpected token in parameter " ^ Lexer.token_string t))
  in
  aux stream { name = ""; has_type = true; type_name = "";}

let read_parameters stream =
  let rec aux st ret =
    let t, s = Lexer.next_token st in
    match t.token_type with
    | Lexer.IDENT ->
      let parameter, s = read_parameter st in
      aux s (parameter :: ret)
    | Lexer.PUNCTUATION COMMA -> aux s ret (* read_parameter finished *)
    | Lexer.PUNCTUATION RPAREN -> List.rev ret, s
    | _ -> raise (ParserError ("Unexpected token in function parameter expression " ^ Lexer.token_string t))
  in
  aux stream []

type function_definition =
  { name: string
    ; parameters : parameter list
    ; has_return_type: bool
    ; return_type: string
    ; access_modifier: access_modifier
  }

let read_function stream access_modifier =
  let is_first i = i.name = "" in
  let rec aux st ret =
    let t, s = Lexer.next_token st in
    match t.token_type with
    | Lexer.IDENT when (is_first ret)-> aux s { ret with name = t.value }
    | Lexer.PUNCTUATION LPAREN ->
      let parameters, s = read_parameters s in
      aux s { ret with parameters = parameters }
    | Lexer.KEYWORD AS -> aux s { ret with has_return_type = true }
    | Lexer.PUNCTUATION LBRACE -> aux s ret
    | Lexer.PUNCTUATION RBRACE -> ret, s
    | _ -> raise (ParserError ("Unexpected token in function expression " ^ Lexer.token_string t))
  in
  aux stream { name = ""; parameters = []; has_return_type = false; return_type = ""; access_modifier = access_modifier }

type class_definition =
  { class_name: string
    ; extends: bool
    ; extends_class: Lexer.token list
    ; private_vars: var list
    ; public_vars: var list
    ; private_functions: function_definition list
    ; public_functions: function_definition list
  }

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
    | _ -> raise (ParserError ("Unexpected token in class definition " ^ Lexer.token_string t))
in
  aux stream class_def

let read_class_block stream class_def = 
  let rec aux str ret =
  let t, st = Lexer.next_token str in
    match t.token_type with

    | Lexer.KEYWORD PRIVATE -> 
      (
        let t, s = Lexer.next_token st in
        match t.token_type with
        | Lexer.KEYWORD VAR ->
          let var, s = read_var s PRIVATE in
          aux s { ret with private_vars = var :: ret.private_vars }
        | Lexer.KEYWORD FUNCTION ->
          let f, s = read_function st PRIVATE in
          aux s { ret with private_functions = f :: ret.private_functions }
        | _ -> raise (ParserError ("Unexpected token in private declaration " ^ Lexer.token_string t))
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
        | _ -> raise (ParserError ("Unexpected token in private declaration " ^ Lexer.token_string t))
      )

    | Lexer.PUNCTUATION LBRACE -> { ret with extends_class = List.rev ret.extends_class; private_vars = List.rev ret.private_vars }, st
    | _ -> raise (ParserError ("Unexpected token in class block " ^ Lexer.token_string t))
in
  aux stream class_def

let read_class stream =
  let class_def = { class_name = ""; extends = false; extends_class = []; public_vars = []; private_vars = []; public_functions = []; private_functions = []} in
  let class_def, s = read_class_definition stream class_def in
  let class_def, s = read_class_block s class_def in
  class_def, s

let print_class_definitiion class_def =
  print_endline (
    "CLASS " ^ class_def.class_name
  );


type p =
  { name: statement
    ; identifer: string
    ; token_list: Lexer.token list
  }

let read_using_expression stream =
  let rec aux st exp =
  let t, s = Lexer.next_token st in
    match t.token_type with
    | Lexer.IDENT -> aux s { exp with token_list = t :: exp.token_list }
    | Lexer.PUNCTUATION DOT -> aux s { exp with token_list = t :: exp.token_list }
    | Lexer.PUNCTUATION SEMICOLON -> { exp with token_list = List.rev exp.token_list }, s
    | _ -> raise (ParserError ("Unexpected token in using expression " ^ Lexer.token_string t))
  in
  aux stream { name = EXPRESSION USING; identifer= ""; token_list = [] }

let read_enum_expression stream = 
  let is_first exp = exp.identifer = "" in
  let rec aux st exp =
  let t, s = Lexer.next_token st in
    match t.token_type with
    | Lexer.IDENT when (is_first exp) -> aux s { exp with identifer = t.value }
    | Lexer.PUNCTUATION LBRACE -> aux s exp
    | Lexer.IDENT -> aux s { exp with token_list = t :: exp.token_list }
    | Lexer.PUNCTUATION COMMA -> aux s exp
    | Lexer.PUNCTUATION RBRACE -> { exp with token_list = List.rev exp.token_list }, s
    | _ -> raise (ParserError ("Unexpected token in enum expression " ^ Lexer.token_string t))
  in
  aux stream { name = ENUM; identifer = ""; token_list = [] }

let print_expression exp = 
  match exp.name with
  | EXPRESSION USING -> print_endline ("USING " ^ string_of_int (List.length exp.token_list)); Lexer.print_token_list exp.token_list
  | ENUM -> print_endline ("ENUM " ^ exp.identifer ^ " " ^ string_of_int (List.length exp.token_list)); Lexer.print_token_list exp.token_list
  | DEFINITION CLASS -> print_endline ("CLASS " ^ exp.identifer ^ " " ^ string_of_int (List.length exp.token_list)); Lexer.print_token_list exp.token_list

let run_next file =
  let stream = Lexer.make file in
  let rec aux str =
    let t, st = Lexer.next_token str in
    match t.token_type with
    | Lexer.EOF -> print_endline "EOF"
    | Lexer.KEYWORD USING -> 
      let use_exp, s = read_using_expression st in
      print_expression use_exp; aux s
    | Lexer.KEYWORD ENUM -> 
      let enum_exp, s = read_enum_expression st in
      print_expression enum_exp; aux s
    | Lexer.KEYWORD CLASS -> 
      let class_def, s = read_class st in
      print_class_definitiion class_def; aux s
    | _ -> print_endline "Undefined_recursive_module"
  in
  aux stream
