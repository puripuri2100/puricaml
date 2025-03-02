%{
  open Machine
%}

%token <int> M_INT     // 0, 1, 2, ...
%token <string> M_STRING_LITERAL // "abc"

%token M_COMMA
%token M_SEMICOLON

(* 括弧類 *)
%token M_LPAREN
%token M_RPAREN
%token M_LBRA
%token M_RBRA
  
(* プリミティブなトークン *)
%token M_TRUE
%token M_FALSE

(* 命令列 *)
%token M_LDI
%token M_LDB
%token M_LDS
%token M_LDL
%token M_ACCESS
%token M_CLOSURE
%token M_LET
%token M_END_LET
%token M_TEST
%token M_ADD
%token M_MINUS
%token M_TIMES
%token M_DIV
%token M_EQ
%token M_LESS
%token M_GREATER
%token M_NOT_EQ
%token M_NOT
%token M_CONS
%token M_HEAD
%token M_TAIL
%token M_LIST_LENGTH
%token M_RAISE
%token M_FAIL_WITH
%token M_TRY
%token M_APPLY
%token M_TAIL_APPLY
%token M_PUSH_MARK
%token M_GRAB
%token M_RETURN

%token M_EOF 

%left M_STRING_LITERAL M_INT M_TRUE M_FALSE M_LBRA M_LPAREN

%start cam
%type <Machine.cam_code> cam

%start zam
%type <Machine.zam_code> zam

%%

zam:
  | zam_code M_EOF { $1 }
;

zam_code:
  | zam_instr { [$1] }
  | zam_instr M_SEMICOLON { [$1] }
  | zam_instr M_SEMICOLON zam_code { $1 :: $3 }
;

zam_list:
  | M_LPAREN zam_code M_RPAREN { [$2] }
  | M_LPAREN zam_code M_RPAREN M_SEMICOLON { [$2] }
  | M_LPAREN zam_code M_RPAREN zam_list { $2 :: $4 }
;

zam_instr:
  | M_LDI M_INT
    { ZAM_Ldi($2) }
  | M_LDB M_TRUE
    { ZAM_Ldb(true) }
  | M_LDB M_FALSE
    { ZAM_Ldb(false) }
  | M_LDS M_STRING_LITERAL
    { ZAM_Lds($2) }
  | M_LDL M_LBRA M_RBRA
    { ZAM_Ldl([]) }
  | M_LDL M_LBRA zam_list M_RBRA
    { ZAM_Ldl($3) }
  | M_ACCESS M_INT { ZAM_Access($2) }
  | M_CLOSURE M_LPAREN zam_code M_RPAREN
    { ZAM_Closure($3) }
  | M_APPLY { ZAM_Apply }
  | M_TAIL_APPLY { ZAM_TailApply }
  | M_PUSH_MARK { ZAM_PushMark }
  | M_GRAB { ZAM_Grab }
  | M_RETURN { ZAM_Return }
  | M_LET { ZAM_Let }
  | M_END_LET { ZAM_EndLet }
  | M_TEST M_LPAREN zam_code M_COMMA zam_code M_RPAREN
    {ZAM_Test($3, $5)}
  | M_EQ { ZAM_Eq }
  | M_NOT_EQ { ZAM_NotEq }
  | M_NOT { ZAM_Not }
  | M_ADD { ZAM_Add }
  | M_MINUS { ZAM_Minus }
  | M_TIMES { ZAM_Times }
  | M_DIV { ZAM_Div }
  | M_GREATER { ZAM_Greater }
  | M_LESS { ZAM_Less }
  | M_CONS { ZAM_Cons }
  | M_HEAD { ZAM_Head }
  | M_TAIL { ZAM_Tail }
  | M_LIST_LENGTH { ZAM_ListLength }
  | M_RAISE { ZAM_Raise }
  | M_FAIL_WITH M_STRING_LITERAL
    { ZAM_FailWith($2) }
  | M_TRY M_LPAREN zam_code M_RPAREN
    { ZAM_Try($3) }
;


cam:
  | cam_code M_EOF { $1 }
;

cam_code:
  | cam_instr { [$1] }
  | cam_instr M_SEMICOLON { [$1] }
  | cam_instr M_SEMICOLON cam_code { $1 :: $3 }
;

cam_list:
  | M_LPAREN cam_code M_RPAREN { [$2] }
  | M_LPAREN cam_code M_RPAREN M_SEMICOLON { [$2] }
  | M_LPAREN cam_code M_RPAREN cam_list { $2 :: $4 }
;

cam_instr:
  | M_LDI M_INT
    { CAM_Ldi($2) }
  | M_LDB M_TRUE
    { CAM_Ldb(true) }
  | M_LDB M_FALSE
    { CAM_Ldb(false) }
  | M_LDS M_STRING_LITERAL
    { CAM_Lds($2) }
  | M_LDL M_LBRA M_RBRA
    { CAM_Ldl([]) }
  | M_LDL M_LBRA cam_list M_RBRA
    { CAM_Ldl($3) }
  | M_ACCESS M_INT { CAM_Access($2) }
  | M_CLOSURE M_LPAREN cam_code M_RPAREN
    {CAM_Closure($3)}
  | M_APPLY { CAM_Apply }
  | M_RETURN
    { CAM_Return }
  | M_LET
    { CAM_Let }
  | M_END_LET
    { CAM_EndLet }
  | M_TEST M_LPAREN cam_code M_COMMA cam_code M_RPAREN
    {CAM_Test($3, $5)}
  | M_EQ { CAM_Eq }
  | M_NOT_EQ { CAM_NotEq }
  | M_NOT { CAM_Not }
  | M_ADD { CAM_Add }
  | M_MINUS { CAM_Minus }
  | M_TIMES { CAM_Times }
  | M_DIV { CAM_Div }
  | M_GREATER { CAM_Greater }
  | M_LESS { CAM_Less }
  | M_CONS { CAM_Cons }
  | M_HEAD { CAM_Head }
  | M_TAIL { CAM_Tail }
  | M_LIST_LENGTH { CAM_ListLength }
  | M_RAISE { CAM_Raise }
  | M_FAIL_WITH M_STRING_LITERAL
    { CAM_FailWith($2) }
  | M_TRY M_LPAREN cam_code M_RPAREN
    { CAM_Try($3) }
;

