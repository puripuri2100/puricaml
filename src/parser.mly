%{
  open Syntax
%}

%token <string> VAR  // x, y, abc, ...
%token <int> INT     // 0, 1, 2, ...
%token <string> STRING_LITERAL // "abc"

%token PLUS     // '+'
%token MINUS    // '-'
%token ASTERISK // '*'
%token SLASH    // '/'
%token NOT_EQUAL// "<>"
%token EQUAL    // '='
%token LESS     // '<'
%token GREATER  // '>'
%token COLCOL   // "::"

%token LPAREN   // '('
%token RPAREN   // ')'
%token LBRA     // '['
%token RBRA     // ']'

%token ARROW    // "->"
%token SEMICOL  // ';'

%token CALL_CC  // "call/cc"
%token RAISE    // "raise"
%token FAIL_WITH// "failwith"

%token TRUE     // "true"
%token FALSE    // "false"
%token FUN      // "fun"
%token LET      // "let"
%token NOT      // "not"
%token REC      // "rec"
%token IN       // "in"
%token IF       // "if"
%token THEN     // "then"
%token WITH     // "with"
%token TRY      // "try"
%token ELSE     // "else"
%token HEAD     // "List.hd"
%token TAIL     // "List.tl"
%token LIST_LENGTH // "List.length"

%token EOF 

%nonassoc IN ELSE ARROW WITH
%left EQUAL GREATER LESS
%right COLCOL
%left PLUS MINUS
%left ASTERISK SLASH
%nonassoc UNARY

%left VAR INT TRUE FALSE LBRA LPAREN

%start main
%type <Syntax.exp> main

%%

main:
  | exp EOF
    { $1 }
;

list_inner:
  | exp { Cons($1, Empty) }
  | exp SEMICOL { Cons($1, Empty) }
  | exp SEMICOL list_inner { Cons($1, $3) }

arg_exp:
  | VAR
    { Var $1 }
    
  | INT
    { IntLit $1 }
    
  | TRUE
    { BoolLit true }
    
  | FALSE
    { BoolLit false }

  | STRING_LITERAL
    { StringLit $1 }
  
  | LBRA RBRA { Empty }
  
  | LBRA list_inner RBRA { $2 }
  
  | LPAREN exp RPAREN
    { $2 }
;

exp:
  | arg_exp
    { $1 }
    
  // (e1 e2)
  | exp arg_exp
    { App ($1, $2) }

  // call/cc e1
  // call/cc (fun k -> e)
  | CALL_CC exp
    { Callcc $2 }

  | RAISE { Raise }
  | FAIL_WITH STRING_LITERAL { FailWith $2 }
  
  | TRY exp WITH exp {Try($2, $4)}
  
  (* not e *)
  | NOT exp
    {Not($2)}
  //  -e
  | MINUS exp %prec UNARY
    { Minus (IntLit 0, $2) }
  
  // e1 + e2
  | exp PLUS exp
    { Plus ($1, $3) }
  
  // e1 - e2
  | exp MINUS exp
    { Minus ($1, $3) }
  
  // e1 * e2
  | exp ASTERISK exp
    { Times ($1, $3) }
  
  // e1 / e2
  | exp SLASH exp
    { Div ($1, $3) }
    
  // e1 <> e2
  | exp NOT_EQUAL exp
    { NotEq ($1, $3) }

  // e1 = e2
  | exp EQUAL exp
    { Eq ($1, $3) }
  
  // e1 < e2
  | exp LESS exp
    { Less ($1, $3) }
    
  // e1 > e2
  | exp GREATER exp
    { Greater ($1, $3) }
    
  // e1 :: e2
  | exp COLCOL exp
    { Cons ($1, $3) }
    
  // List.hd e
  | HEAD arg_exp
    { Head $2 }
    
  // List.tl e
  | TAIL arg_exp
    { Tail $2 }

  // List.length e
  | LIST_LENGTH arg_exp
    { ListLength $2 }
  
  // fun x -> e
  | FUN VAR ARROW exp
    { Fun ($2, $4) }
  
  // let x = e1 in e2
  | LET VAR EQUAL exp IN exp
    { Let ($2, $4, $6) }
  
  // let rec f x = e1 in e2
  | LET REC VAR VAR EQUAL exp IN exp
    { LetRec ($3, $4, $6, $8) }
  
  // if e1 then e2 else e3
  | IF exp THEN exp ELSE exp
    { If ($2, $4, $6) }
  
  | error
    { 
      let message =
        Printf.sprintf 
          "parse error near characters %d-%d"
          (Parsing.symbol_start ())
          (Parsing.symbol_end ())
        in
      failwith message
    }
;
