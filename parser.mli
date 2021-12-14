type token =
  | LAMBDA
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | SUCC
  | PRED
  | ISZERO
  | LET
  | LETREC
  | IN
  | CONCAT
  | BOOL
  | NAT
  | STR
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | FST
  | SND
  | DOT
  | EQ
  | COLON
  | ARROW
  | QUOTE
  | EOF
  | INTV of (int)
  | STRINGV of (string)
  | STRING of (string)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.operations
