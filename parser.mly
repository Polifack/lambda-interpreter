
%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token IN
%token CONCAT
%token BOOL
%token NAT
%token STR

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token COMMA
%token FST
%token SND
%token TL
%token HD
%token ISEMPT
%token DOT
%token EQ
%token COLON
%token ARROW
%token QUOTE
%token EOF

%token <int> INTV
%token <string> STRINGV
%token <string> STRING

%start s
%type <Lambda.operations> s

%%

s:
    STRINGV EQ term EOF
        { Bind ($1, $3) }
    | term EOF
        { Eval $1 }
        
term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA STRINGV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET STRINGV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LET STRINGV COLON ty EQ term IN term
      { TmLetIn ($2, TmAbs ($2, $4, $6), $8) }
  | LETREC STRINGV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix ( TmAbs ($2, $4, $6)), $8) }
  | CONCAT term term
      { TmConcat ($2,$3) }

appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
  | appTerm atomicTerm
      { TmApp ($1, $2) }
  | appTerm CONCAT appTerm
      { TmConcat ($1,$3) }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | STRINGV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | STRING 
      { 
        TmString (String.sub $1 1 (String.length($1)-2))
      }
  | LBRACE recordTerm RBRACE
      { TmRecord $2 }
  | LBRACKET listTerm RBRACKET
      { TmList $2 }
  | LBRACKET RBRACKET
      { TmEmptyList }
  | atomicTerm DOT STRINGV
      { TmProjection ($1, $3) }
  | pairTerm
      { $1 }
  | atomicTerm FST
      { TmFst $1 }
  | atomicTerm SND
      { TmSnd $1 }
  | HD atomicTerm
      { TmHead $2 }
  | TL atomicTerm
      { TmTail $2 }
  | ISEMPT atomicTerm
      {  TmIsEmpty $2 }

pairTerm :
  | LPAREN term COMMA term RPAREN
      { TmPair ($2,$4) }
      
recordTerm :
    STRINGV COLON term
      { [($1,$3)] }
    | STRINGV COLON term COMMA recordTerm
      { ($1,$3)::$5 }

listTerm :
    term
      { [$1] }
    | term COMMA listTerm
      { $1::$3 }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }

atomicTy :
    LPAREN ty RPAREN  
      { $2 } 
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STR
      { TyString }
  | pairTy 
      { $1 }
  | LBRACE recordTy RBRACE
      { TyRecord $2 }
  | LBRACKET listTy RBRACKET
      { TyList $2 }

  
pairTy:
  | LPAREN ty COMMA ty RPAREN
      { TyPair($2,$4)}

recordTy:
    STRINGV COLON ty
      { [($1,$3)] }
   | STRINGV COLON ty COMMA recordTy
      { ($1,$3)::$5 }

listTy :
    ty
      { $1 }


