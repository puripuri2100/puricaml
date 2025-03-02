{
  open Machine_parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = digit | alpha | '\''


rule token = parse
  | digit+
    { let str = Lexing.lexeme lexbuf in
      M_INT (int_of_string str) }

  | '"'       {
    let buf = Buffer.create 256 in
    let str = str_literal buf lexbuf in
    M_STRING_LITERAL(str)
  }
  
  (* 記号 *)
  | ','       { M_COMMA }
  | ';'       { M_SEMICOLON }

  (* 括弧類 *)
  | '('       { M_LPAREN }
  | ')'       { M_RPAREN }
  | '['       { M_LBRA }
  | ']'       { M_RBRA }
  
  (* プリミティブなトークン *)
  | "true"    { M_TRUE }
  | "false"   { M_FALSE }

  (* 命令列 *)
  | "Ldi"     { M_LDI }
  | "Ldb"     { M_LDB }
  | "Lds"     { M_LDS }
  | "Ldl"     { M_LDL }
  | "Access"  { M_ACCESS }
  | "Closure" { M_CLOSURE }
  | "Let"     { M_LET }
  | "EndLet"  { M_END_LET }
  | "Test"    { M_TEST }
  | "Add"     { M_ADD }
  | "Minus"   { M_MINUS }
  | "Times"   { M_TIMES }
  | "Div"     { M_DIV }
  | "Eq"      { M_EQ }
  | "Less"    { M_LESS }
  | "Greater" { M_GREATER }
  | "NotEq"   { M_NOT_EQ }
  | "Not"     { M_NOT }
  | "Cons"    { M_CONS }
  | "Head"    { M_HEAD }
  | "Tail"    { M_TAIL }
  | "ListLength"    { M_LIST_LENGTH }
  | "Raise"   { M_RAISE }
  | "FailWith"{ M_FAIL_WITH }
  | "Try"     { M_TRY }
  | "Apply"   { M_APPLY }
  | "TailApply"{ M_TAIL_APPLY }
  | "PushMark"{ M_PUSH_MARK }
  | "Grab"    { M_GRAB }
  | "Return"  { M_RETURN }

  (* 終端 *)
  | eof       { M_EOF }

  (* 空白は無視 *)
  | space+    { token lexbuf }

  | _
    {
      let message = Printf.sprintf
        "unknown token %s near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)
      in
      failwith message
    }

and str_literal buffer = parse
  | '"' {
    Buffer.contents buffer
  }
  | _ {
    let tok = Lexing.lexeme lexbuf in
    let () = Buffer.add_string buffer tok in
    str_literal buffer lexbuf
  }