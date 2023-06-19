type punctuation =
  | LBRACE
  | RBRACE
  | LBRACKET of (string)
  | RBRACKET
  | LPAREN
  | RPAREN
  | COMMA
  | SEMICOLON
  | DOT
  | COLON
  | ARROW
  | AT

type number =
  | NUMBER of (int)
  | LONG of (int64)
  | FLOAT of (float)
  | DOUBLE of (float)

type operator =
  | EQ
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
  | NEQ

type keyword =
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

type tokens =
  | IDENT of (string)
  | STRING of (string)
  | BOOLEAN of (bool)
  | CHAR of (char)
  | NUMBER of (number)
  | PUNCTUATION of (punctuation)
  | OPERATOR of (operator)
  | KEYWORD of (keyword)
  | EOF


(* class lex s = *)
(*   object (_) *)
(*   val current = ref "" *)
(*   method read_next = *)
(*     let rec read_next_aux () = *)
(*       let c = s#next in *)
(*       if c = '\n' then *)
(*         current := "" *)
(*       else if c = ' ' then *)
(*         current := "" *)
(*       else if c = '\t' then *)
(*         current := "" *)
(*       else *)
(*         current := !current ^ (String.make 1 c); *)

(*       if s#eof = true then *)
(*         EOF *)
(*       else *)
(*         read_next_aux () *)
(*     in *)
(*     read_next_aux () *)
(* end;; *)

let make s = Io.InputStream.make s
let eof l = Io.InputStream.eof l
let read_next l =
  let next =
    match Io.InputStream.next l with
    | s, '\n' -> { s with current = "" }
    | s, ' ' -> { s with current = "" }
    | s, '\t' -> { s with current = "" }
    | s, c -> { s with current = s.current ^ (String.make 1 c) }
  in
  if eof l then EOF
  else let current = next.current
    in
    match current with
    | "(" -> PUNCTUATION(LBRACKET("("))
    | s -> IDENT(s);
