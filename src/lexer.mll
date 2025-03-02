{
  open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = digit | alpha | '\''


rule token = parse
  | digit+
    { let str = Lexing.lexeme lexbuf in
      INT (int_of_string str) }

  | '"'       {
    let buf = Buffer.create 256 in
    let str = str_literal buf lexbuf in
    STRING_LITERAL(str)
  }
  
  (* 記号 *)
  | '+'       { PLUS }
  | '-'       { MINUS }
  | '*'       { ASTERISK }
  | '/'       { SLASH }
  | '='       { EQUAL }
  | "<>"      { NOT_EQUAL }
  | '<'       { LESS }
  | '>'       { GREATER }
  | ';'       { SEMICOL }
  | "::"      { COLCOL }

  (* 括弧類 *)
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | '['       { LBRA }
  | ']'       { RBRA }
  | "->"      { ARROW }
  
  (* プリミティブなトークン *)
  | "call/cc" { CALL_CC }
  | "raise"   { RAISE }
  | "failwith"{ FAIL_WITH } (* 将来文字列型入れるときはメッセージを着けられるようにする *)
  | "true"    { TRUE }
  | "false"   { FALSE }
  | "not"     { NOT }
  | "fun"     { FUN }
  | "let"     { LET }
  | "rec"     { REC }
  | "in"      { IN }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "with"    { WITH }
  | "try"     { TRY }
  | "List.hd" { HEAD }
  | "List.tl" { TAIL }
  | "List.length" { LIST_LENGTH }

  (* 変数名 *)
  | alpha alnum*
    { VAR (Lexing.lexeme lexbuf) }
  
  (* 終端 *)
  | eof       { EOF }

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