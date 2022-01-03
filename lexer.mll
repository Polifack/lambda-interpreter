
{
  open Parser;;
  exception Lexical_error;; 
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "letrec"    { LETREC }
  | "in"        { IN }
  | "@"         { CONCAT }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "Str"       { STR }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '{'         { LBRACE }
  | '}'         { RBRACE }
  | '['         {LBRACKET}
  | ']'         { RBRACKET}
  | ','         { COMMA }
  | '.'         { DOT }
  | ".1"        { FST }
  | ".2"        { SND }
  | "hd"        { HD }
  | "tl"        { TL }
  | "isempty"  { ISEMPT }
  | '='         { EQ }
  | ':'         { COLON }
  | "->"        { ARROW }
  | "\""        { QUOTE }
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*               { STRINGV (Lexing.lexeme lexbuf) }
  | '"'['a'-'z' '_' '0'-'9' ' ' '!' '?' '-']*'"'  {  STRING (Lexing.lexeme lexbuf) }
  | eof         { EOF }
  | _           { raise Lexical_error } 

