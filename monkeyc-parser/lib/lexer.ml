type punctuation =
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | LPAREN
  | RPAREN
  | COMMA
  | SEMICOLON
  | DOT
  | COLON
  | QUESTIONMARK

type number =
  | NUMBER
  | LONG
  | FLOAT
  | DOUBLE

type operator =
  | EQ
  | EQEQ
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | AND
  | OR
  | NOT
  | LT
  | GT
  | LTE
  | GTE
  | ARROW
  | NEQ
  | PLUSEQ
  | MINUSEQ
  | PLUSPLUS
  | MINUSMINUS

type keyword =
  | USING
  | CLASS
  | FUNCTION
  | ENUM
  | RETURN
  | BREAK
  | CONTINUE
  | FOR
  | WHILE
  | DO
  | NEW
  | VAR
  | HIDDEN
  | ME
  | MODULE
  | NULL
  | PUBLIC
  | PRIVATE
  | PROTECTED
  | STATIC
  | EXTENDS
  | THROW
  | OR
  | SELF
  | TYPEDEF
  | AS
  | METHOD
  | INSTANEOF
  | HAS
  | IF
  | THEN
  | ELSE
  | ASSERT
  | SWITCH
  | CASE
  | TRY
  | CATCH
  | FINALLY

type token_type =
  | IDENT
  | STRING
  | BOOLEAN of (bool)
  | CHAR of (char)
  | NUMBER
  | PUNCTUATION of (punctuation)
  | OPERATOR of (operator)
  | KEYWORD of (keyword)
  | COMMENT
  | EOF

let is_punc c =
  match c with
  | '(' | ')' | '{' | '}' | '[' | ']' | ';' | '.' | ':' | ',' | '?' -> true
  | _ -> false

let match_punc = function
  | '(' -> LPAREN
  | ')' -> RPAREN
  | '{' -> LBRACE
  | '}' -> RBRACE
  | '[' -> LBRACKET
  | ']' -> RBRACKET
  | ';' -> SEMICOLON
  | '.' -> DOT
  | ':' -> COLON
  | ',' -> COMMA
  | '?' -> QUESTIONMARK
  | _ -> raise (Failure "Not punctuation")

let string_of_operator = function
  | EQ -> "EQ"
  | EQEQ -> "EQEQ"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | DIV -> "DIV"
  | MOD -> "MOD"
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"
  | LT -> "LT"
  | GT -> "GT"
  | LTE -> "LTE"
  | GTE -> "GTE"
  | ARROW -> "ARROW"
  | NEQ -> "NEQ"
  | PLUSEQ -> "PLUSEQ"
  | MINUSEQ -> "MINUSEQ"
  | PLUSPLUS -> "PLUSPLUS"
  | MINUSMINUS -> "MINUSMINUS"

let string_of_keyword = function
  | USING -> "USING"
  | CLASS -> "CLASS"
  | FUNCTION -> "FUNCTION"
  | ENUM -> "ENUM"
  | RETURN -> "RETURN"
  | BREAK -> "BREAK"
  | CONTINUE -> "CONTINUE"
  | FOR -> "FOR"
  | WHILE -> "WHILE"
  | DO -> "DO"
  | NEW -> "NEW"
  | VAR -> "VAR"
  | HIDDEN -> "HIDDEN"
  | ME -> "ME"
  | MODULE -> "MODULE"
  | NULL -> "NULL"
  | PUBLIC -> "PUBLIC"
  | PRIVATE -> "PRIVATE"
  | PROTECTED -> "PROTECTED"
  | STATIC -> "STATIC"
  | EXTENDS -> "EXTENDS"
  | THROW -> "THROW"
  | OR -> "OR"
  | SELF -> "SELF"
  | TYPEDEF -> "TYPEDEF"
  | AS -> "AS"
  | METHOD -> "METHOD"
  | INSTANEOF -> "INSTANEOF"
  | HAS -> "HAS"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | ASSERT -> "ASSERT"
  | SWITCH -> "SWITCH"
  | CASE -> "CASE"
  | TRY -> "TRY"
  | CATCH -> "CATCH"
  | FINALLY -> "FINALLY"

let make s = Io.Input_stream.make s

let take stream =
  match Io.Input_stream.next stream with
  | s, '\n' -> s.current <- ""; s
  | s, ' ' -> s.current <- ""; s
  | s, '\t' -> s.current <- ""; s
  | s, c -> s.current <- (String.make 1 c); s

let read_string stream =
  let rec aux s string =
    match Io.Input_stream.next s with
    | s, '"' -> s, string
    | s, c -> aux s (string ^ (String.make 1 c))
  in
  aux stream ""

let is_number = function
  | '0'..'9' -> true
  | _ -> false

(* TODO: handle floats, longs, doubles *)
let read_number stream =
  let rec aux s number =
    match Io.Input_stream.peek s with
    | None -> s, number
    | Some c when (is_number c) -> aux (take s) (number ^ (String.make 1 c))
    | _ -> s, number
  in
  aux stream ""

let is_ident_start = function
  | 'a'..'z' -> true
  | 'A'..'Z' -> true
  | '_' -> true
  | _ -> false

let read_ident stream =
  let rec aux s ident =
    match Io.Input_stream.peek s with
    | None -> s, ident
    | Some c when (is_ident_start c) -> aux (take s) (ident ^ (String.make 1 c))
    | _ -> s, ident
  in
  aux stream ""

let is_whitespace = function
  | ' ' -> true
  | '\t' -> true
  | '\n' -> true
  | _ -> false

let is_punctuation = function
  | '(' -> true
  | ')' -> true
  | '{' -> true
  | '}' -> true
  | '.' -> true
  | ';' -> true
  | '?' -> true
  | _ -> false

let is_keyword = function
  | "using" -> true
  | "class" -> true
  | "function" -> true
  | "enum" -> true
  | "return" -> true
  | "break" -> true
  | "continue" -> true
  | "for" -> true
  | "while" -> true
  | "do" -> true
  | "new" -> true
  | "var" -> true
  | "hidden" -> true
  | "me" -> true
  | "module" -> true
  | "null" -> true
  | "public" -> true
  | "private" -> true
  | "protected" -> true
  | "static" -> true
  | "extends" -> true
  | "throw" -> true
  | "or" -> true
  | "self" -> true
  | "typedef" -> true
  | "as" -> true
  | "method" -> true
  | "instanceof" -> true
  | "has" -> true
  | "if" -> true
  | "then" -> true
  | "else" -> true
  | "assert" -> true
  | "switch" -> true
  | "case" -> true
  | "try" -> true
  | "catch" -> true
  | "finally" -> true
  | _ -> false

let is_boolean = function
  | "true" -> true
  | "false" -> true
  | _ -> false

let match_bool = function
  | "true" -> true
  | "false" -> false
  | _ -> raise (Failure "Not a boolean")

let match_keyword = function
  | "using" -> USING
  | "class" -> CLASS
  | "function" -> FUNCTION
  | "enum" -> ENUM
  | "return" -> RETURN
  | "break" -> BREAK
  | "continue" -> CONTINUE
  | "for" -> FOR
  | "while" -> WHILE
  | "do" -> DO
  | "new" -> NEW
  | "var" -> VAR
  | "hidden" -> HIDDEN
  | "me" -> ME
  | "module" -> MODULE
  | "null" -> NULL
  | "public" -> PUBLIC
  | "private" -> PRIVATE
  | "protected" -> PROTECTED
  | "static" -> STATIC
  | "extends" -> EXTENDS
  | "throw" -> THROW
  | "or" -> OR
  | "self" -> SELF
  | "typedef" -> TYPEDEF
  | "as" -> AS
  | "method" -> METHOD
  | "instanceof" -> INSTANEOF
  | "has" -> HAS
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "assert" -> ASSERT
  | "switch" -> SWITCH
  | "case" -> CASE
  | "try" -> TRY
  | "catch" -> CATCH
  | "finally" -> FINALLY
  | _ -> raise (Failure "Not a keyword")

let is_op = function
  | '+' -> true
  | '-' -> true
  | '*' -> true
  | '/' -> true
  | '%' -> true
  | '=' -> true
  | '<' -> true
  | '>' -> true
  | '&' -> true
  | '|' -> true
  | '^' -> true
  | '~' -> true
  | '!' -> true
  | _ -> false

let read_op stream =
  let rec aux s op =
    match Io.Input_stream.peek s with
    | Some c when (is_op c) -> aux (take s) (op ^ (String.make 1 c))
    | Some _ -> s, op
    | None -> s, op
  in
  aux stream ""

let match_op = function
  | "+" -> PLUS
  | "-" -> MINUS
  | "=" -> EQ
  | "*" -> TIMES
  | "/" -> DIV
  | "%" -> MOD
  | "<" -> LT
  | ">" -> GT
  | ">=" -> GTE
  | "<=" -> LTE
  | "=>" -> ARROW
  | "==" -> EQEQ
  | "!=" -> NEQ
  | "&&" -> AND
  | "||" -> OR
  | "!" -> NOT
  | "+=" -> PLUSEQ
  | "-=" -> MINUSEQ
  | "++" -> PLUSPLUS
  | "--" -> MINUSMINUS
  | c -> raise (Failure ("Not an operator: " ^ c))

let rec skip_to st c =
  match Io.Input_stream.next st with
  | s, c' when c = c' -> s
  | s, _ -> skip_to s c

let is_comment st =
  let double_peek = match Io.Input_stream.double_peek st with
    | Some c -> String.make 1 c
    | None -> ""
  in
  let sl c = String.make 1 c ^ double_peek = "//" in
  let ml c = String.make 1 c ^ double_peek = "/*" in
  function
  | c when ml c || sl c-> true
  | _ -> false

let is_comment_next st =
  let double_peek = match Io.Input_stream.double_peek st with
    | Some c -> String.make 1 c
    | None -> ""
  in
  let sl c = c ^ double_peek = "//" in
  let ml c = c ^ double_peek = "/*" in
  match st.current with
  | c when ml c || sl c -> true
  | _ -> false

let skip_comment stream =
  let rec aux st =
    let peek s = match Io.Input_stream.peek s with
      | Some c -> String.make 1 c
      | None -> ""
    in
    match Io.Input_stream.next st with
    | s, c when String.make 1 c ^ peek s = "*/" -> take s
    | s, c when String.make 1 c ^ peek s = "//" -> skip_to s '\n'
    | s, _ -> aux s
  in
  aux stream

type token =
  { token_type : token_type
    ; value : string
    ; line : int
    ; col : int
  }

let next_token stream =
  let eof st = Io.Input_stream.eof st in
  let rec aux st =
    if eof st then { token_type = EOF; value = ""; line = st.line; col = st.col }, st
    else let peek = Io.Input_stream.peek st in
      match peek with
      | None -> raise (Failure "Unexpected end of file")
      | Some c when (is_whitespace c) -> aux (take st)
      | Some c when (is_comment stream c) -> aux (skip_comment st)

      | Some '"' ->
        let s, string = read_string (take st) in
        { token_type = STRING; value = string; line = s.line; col = s.col}, s

      | Some c when (is_number c) ->
        let s, number = read_number st in
        { token_type = NUMBER; value = number; line = s.line; col = s.col}, s

      | Some c when (is_ident_start c) ->
        let s, string = read_ident st in
        if (is_keyword string) then
          { token_type = KEYWORD (match_keyword string); value = string; line = s.line; col = s.col}, s
        else if (is_boolean string) then
          { token_type = BOOLEAN (match_bool string); value = string; line = s.line; col = s.col}, s
        else
          { token_type = IDENT; value = string; line = s.line; col = s.col}, s

      | Some c when (is_op c) ->
        let s, op = read_op st in
        { token_type = OPERATOR (match_op op); value = op; line = s.line; col = s.col}, s

      | Some c when (is_punc c) ->
        { token_type = PUNCTUATION (match_punc c); value = (String.make 1 c); line = st.line; col = st.col}, (take st)

      | Some c -> { token_type = IDENT; value = (String.make 1 c); line = st.line; col = st.col}, (take st)
  in
  aux stream


let tokens stream =
  let eof stream = Io.Input_stream.eof stream in
  let rec aux st (acc: token list) =
    if eof st then acc
    else let peek = Io.Input_stream.peek st in
      match peek with
      | None -> raise (Failure "Unexpected end of file")
      | Some c when (is_whitespace c) -> aux (take st) acc
      | Some c when (is_comment st c) -> aux (skip_comment st) (
        { token_type = COMMENT; value = ""; line = st.line; col = st.col}
        :: acc)

      | Some '"' ->
        ( let s, string = read_string (take st) in
          aux s ((
            { token_type = STRING; value = string; line = s.line; col = s.col}
          ) :: acc))

      | Some c when (is_number c) -> (
        let s, number = read_number st in
        aux s ((
          { token_type = NUMBER; value = number; line = s.line; col = s.col}
        ) :: acc))

      | Some c when (is_ident_start c) ->
        ( let s, string = read_ident st in
          if (is_keyword string) then
            aux s (
              { token_type = KEYWORD (match_keyword string); value = string; line = s.line; col = s.col}
              :: acc)
          else if (is_boolean string) then
            aux s ((
              { token_type = BOOLEAN (match_bool string); value = string; line = s.line; col = s.col}
            ) :: acc)
          else
            aux s ((
              { token_type = IDENT; value = string; line = s.line; col = s.col}
            ) :: acc))

      | Some c when (is_op c) -> (
        let s, op = read_op st in
        aux s (
          { token_type = OPERATOR (match_op op); value = op; line = s.line; col = s.col}
          :: acc))

      | Some c when (is_punc c) -> (
        aux (take st) (
          { token_type = PUNCTUATION (match_punc c); value = (String.make 1 c); line = st.line; col = st.col}
          :: acc))

      | Some c -> aux (take st) (
        { token_type = IDENT; value = (String.make 1 c); line = st.line; col = st.col}
        :: acc)
  in
  List.rev (aux stream [])

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

let print_token t = 
    print_endline (token_string t)

let print_token_list l = 
  List.iter (fun ( t : token) ->
    print_token t
  ) l
