type exp = 
  | Var of string         (* variable e.g. x *)
  | IntLit of int         (* integer literal e.g. 17 *)
  | BoolLit of bool       (* boolean literal e.g. true *)
  | StringLit of string   (* string literal e.g. "abc" *)
  | If of exp * exp * exp (* if e then e else e *)
  | Let of string * exp * exp   (* let x=e in e *)
  | LetRec of string * string * exp * exp   (* letrec f x=e in e *)
  | Fun of string * exp   (* fun x -> e *)
  | App of exp * exp      (* function application i.e. e e *)
  | Callcc of exp         (* (call/cc e), (call/cc (fun k -> e)) *)
  | Eq of exp * exp       (* e = e *)
  | NotEq of exp * exp    (* e <> e *)
  | Not of exp            (* not e *)
  | Greater of exp * exp  (* e > e *)
  | Less of exp * exp     (* e < e *)
  | Plus of exp * exp     (* e + e *)
  | Minus of exp * exp    (* e - e *)
  | Times of exp * exp    (* e * e *)
  | Div of exp * exp      (* e / e *)
  | Empty                 (* [ ] *)
  | Cons of exp * exp     (* e :: e *)
  | Head of exp           (* List.hd e *)
  | Tail of exp           (* List.tl e *)
  | ListLength of exp     (* List.length e *)
  | Raise                 (* Raise *)
  | FailWith of string    (* failwith "err msg" *)
  | Try of exp * exp      (* try e with handler *)

  let rec gen_indent dep =
    if dep == 0 then
      ""
    else
      "  " ^ gen_indent (dep - 1)

  let rec print_exp dep e =
    let indent = gen_indent dep in
    match e with
    | Var(s) -> Printf.sprintf "%sVar(%s)" indent s
    | IntLit(n) -> Printf.sprintf "%sIntLit(%d)" indent n
    | BoolLit(true) -> Printf.sprintf "%sBoolLit(true)" indent
    | BoolLit(false) -> Printf.sprintf "%sBoolLit(true)" indent
    | StringLit(str) -> Printf.sprintf "%sStringLit(%s)" indent str
    | If(b, e2, e3) ->
      Printf.sprintf "%sIf(\n%s,\n%s\n%s\n%s)" indent (print_exp (dep + 1) b) (print_exp (dep + 1) e2) (print_exp (dep + 1) e3) indent
    | Let(s, e2, e3) ->
      Printf.sprintf "%sLet(\n%s%s,\n%s\n%s\n%s)" indent indent s (print_exp (dep + 1) e2) (print_exp (dep + 1) e3) indent
    | LetRec(s1, s2, e2, e3) ->
      Printf.sprintf "%sLetRec(\n%s%s,\n%s%s,\n%s\n%s\n%s)" indent indent s1 indent s2 (print_exp (dep + 1) e2) (print_exp (dep + 1) e3) indent
    | Fun(s, e) -> Printf.sprintf "%sFun(\n%s%s,\n%s\n%s)" indent indent s (print_exp (dep + 1) e) indent
    | App(e1, e2) -> Printf.sprintf "%sApp(\n%s,\n%s\n%s)" indent (print_exp (dep + 1) e1) (print_exp (dep + 1) e2) indent
    | Callcc(e) -> Printf.sprintf "%sCallcc(\n%s%s\n%s)" indent indent (print_exp (dep + 1) e) indent
    | NotEq(e1, e2) -> Printf.sprintf "%sNotEq(\n%s,\n%s\n%s)" indent (print_exp (dep + 1) e1) (print_exp (dep + 1) e2) indent
    | Eq(e1, e2) -> Printf.sprintf "%sEq(\n%s,\n%s\n%s)" indent (print_exp (dep + 1) e1) (print_exp (dep + 1) e2) indent
    | Not(e) -> Printf.sprintf "%sNot(\n%s\n%s)" indent (print_exp (dep + 1) e) indent
    | Greater(e1, e2) -> Printf.sprintf "%sGreater(\n%s,\n%s\n%s)" indent (print_exp (dep + 1) e1) (print_exp (dep + 1) e2) indent
    | Less(e1, e2) -> Printf.sprintf "%sLess(\n%s,\n%s\n%s)" indent (print_exp (dep + 1) e1) (print_exp (dep + 1) e2) indent
    | Plus(e1, e2) -> Printf.sprintf "%sPlus(\n%s,\n%s\n%s)" indent (print_exp (dep + 1) e1) (print_exp (dep + 1) e2) indent
    | Minus(e1, e2) -> Printf.sprintf "%sMinus(\n%s,\n%s\n%s)" indent (print_exp (dep + 1) e1) (print_exp (dep + 1) e2) indent
    | Times(e1, e2) -> Printf.sprintf "%sTimes(\n%s,\n%s\n%s)" indent (print_exp (dep + 1) e1) (print_exp (dep + 1) e2) indent
    | Div(e1, e2) -> Printf.sprintf "%sDiv(\n%s,\n%s\n%s)" indent (print_exp (dep + 1) e1) (print_exp (dep + 1) e2) indent
    | Empty -> Printf.sprintf "%sEmpty" indent
    | Cons(e1, e2) -> Printf.sprintf "%sCons(\n%s,\n%s\n%s)" indent (print_exp (dep + 1) e1) (print_exp (dep + 1) e2) indent
    | Head(e) -> Printf.sprintf "%sHead(\n%s\n%s)" indent (print_exp (dep + 1) e) indent
    | Tail(e) -> Printf.sprintf "%sTail(\n%s\n%s)" indent (print_exp (dep + 1) e) indent
    | ListLength(e) -> Printf.sprintf "%sListLength(\n%s\n%s)" indent (print_exp (dep + 1) e) indent
    | Raise -> Printf.sprintf "%sRaise" indent
    | FailWith(s) -> Printf.sprintf "%sFailWith(%s)" indent s
    | Try(e1, e2) -> Printf.sprintf "%sTry(\n%s%s,\n%s%s\n%s)" indent indent (print_exp (dep + 1) e1) indent (print_exp (dep + 1) e2) indent

type value = 
  | IntVal  of int        (* integer value e.g. 17 *)
  | BoolVal of bool       (* booleanvalue e.g. true *)
  | StringVal of string   (* string *)
  | ListVal of value list (* list value e.g. [1;2;3] *)
  | FunVal  of string * exp * env
                          (* function value e.g. \x. x+1 with env *)
  | RecFunVal of string * string * exp * env
                          (* recursive function value: solution-1 *)
                          (* let rec f x = e1 in e2 *)
  | ContVal of (value -> value) (* call/cc e1 や call/cc (fun k -> e) など　*)
  | Fail of string
and
  env = (string * value) list


let rec print_value dep value =
  let indent = gen_indent dep in
  match value with
  | IntVal(n) -> Printf.sprintf "%sIntVal(%d)" indent n
  | BoolVal(true) -> Printf.sprintf "%sBoolVal(true)" indent
  | BoolVal(false) -> Printf.sprintf "%sBoolVal(false)" indent
  | StringVal(str) -> Printf.sprintf "%sStringVal(%s)" indent str
  | ListVal(lst) ->
    let rec print_value_lst dep lst =
      let indent = gen_indent dep in
      match lst with
      | [] -> ""
      | [v] -> Printf.sprintf "%s%s;\n" indent (print_value (dep + 1) v)
      | v::tail ->
        Printf.sprintf "%s%s;\n" indent (print_value (dep + 1) v)
          ^ (print_value_lst dep tail)
    in
    Printf.sprintf "%sListVal([\n%s%s])" indent (print_value_lst dep lst) indent
  | FunVal(s, exp, env) ->
    Printf.sprintf "%sFunVal(\n%s,\n%s,\n%s\n%s)" indent s (print_exp (dep + 1) exp) (print_env (dep + 1) env) indent
  | RecFunVal(s1, s2, exp, env) ->
    Printf.sprintf "%sRecFunVal(\n%s,\n%s,\n%s,\n%s\n%s)" indent s1 s2 (print_exp (dep + 1) exp) (print_env (dep + 1) env) indent
  | ContVal(_) -> Printf.sprintf "%sContVal" indent
  | Fail(msg) -> Printf.sprintf "%sFail(%s)" indent msg

and print_env dep env =
  let indent = gen_indent dep in
  let rec f dep lst =
    let indent = gen_indent dep in
    match lst with
    | [] -> ""
    | [(key, value)] -> Printf.sprintf "%s(\n%s,%s\n%s)\n" indent key (print_value (dep + 1) value) indent
    | (key, value)::tail ->
      Printf.sprintf "%s(\n%s,%s\n%s)\n" indent key (print_value (dep + 1) value) indent ^ f dep tail
  in
  Printf.sprintf "%s[\n%s%s]" indent (f dep env) indent

