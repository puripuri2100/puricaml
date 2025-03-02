open Syntax
open Machine

let rec zam_value_eq v1 v2 =
  match (v1, v2) with
  | (ZAM_IntVal(n1), ZAM_IntVal(n2)) -> Some(n1 = n2)
  | (ZAM_BoolVal(b1), ZAM_BoolVal(b2)) -> Some(b1 = b2)
  | (ZAM_StringVal(b1), ZAM_StringVal(b2)) -> Some(b1 = b2)
  | (ZAM_ListVal(l1), ZAM_ListVal(l2)) -> (
    let rec f l1 l2 =
      match (l1, l2) with
      | (h1::tail1, h2::tail2) -> (
        match (zam_value_eq h1 h2, f tail1 tail2) with
        | (Some(b1), Some(b2)) -> Some(b1 && b2)
        | _ -> None
      )
      | ([], []) -> Some(true)
      | _ -> None
    in
    f l1 l2
  )
  | _ -> None


let rec run_zam_code_sub (code: zam_code) (env: zam_env) (s: zam_stack) (r: zam_stack): zam_value =
  match code with
  | ZAM_Ldi(n)::c -> run_zam_code_sub c env (ZAM_IntVal(n)::s) r
  | ZAM_Ldb(b)::c -> run_zam_code_sub c env (ZAM_BoolVal(b)::s) r
  | ZAM_Lds(str) :: c -> run_zam_code_sub c env (ZAM_StringVal(str)::s) r
  | ZAM_Ldl(l) :: c -> (
    let l = List.map (fun c -> run_zam_code_sub c env s r) l in
    run_zam_code_sub c env (ZAM_ListVal(l)::s) r
  )
  | ZAM_Access(i)::c -> run_zam_code_sub c env ((List.nth env i) :: s) r
  | ZAM_Closure(cl) :: c -> run_zam_code_sub c env (ZAM_ClosVal(cl, env)::s) r
  | ZAM_Let::c -> (
    match s with
    | v::s -> run_zam_code_sub c (v::env) s r
    | _ -> failwith "error at ZAM_Let"
  )
  | ZAM_EndLet::c -> (
    match env with
    | _::env -> run_zam_code_sub c env s r
    | _ -> failwith "error at ZAM_EndLet"
  )
  | ZAM_Test(c1, c2)::c -> (
    match s with
    | ZAM_BoolVal(true) :: s -> run_zam_code_sub (c1@c) env s r
    | ZAM_BoolVal(false) :: s -> run_zam_code_sub (c2@c) env s r
    | _ -> failwith "error at ZAM_Test"
  )
  | ZAM_Add::c -> (
    match s with
    | ZAM_IntVal(n1)::ZAM_IntVal(n2)::s -> run_zam_code_sub c env (ZAM_IntVal(n1 + n2)::s) r
    | _ -> failwith "error at ZAM_Add"
  )
  | ZAM_Minus::c -> (
    match s with
    | ZAM_IntVal(n1)::ZAM_IntVal(n2)::s -> run_zam_code_sub c env (ZAM_IntVal(n1 - n2)::s) r
    | _ -> failwith "error at ZAM_Minus"
  )
  | ZAM_Times::c -> (
    match s with
    | ZAM_IntVal(n1)::ZAM_IntVal(n2)::s -> run_zam_code_sub c env (ZAM_IntVal(n1 * n2)::s) r
    | _ -> failwith "error at ZAM_Times"
  )
  | ZAM_Div::c -> (
    match s with
    | ZAM_IntVal(n1)::ZAM_IntVal(n2)::s -> run_zam_code_sub c env (ZAM_IntVal(n1 / n2)::s) r
    | _ -> failwith "error at ZAM_Div"
  )
  | ZAM_Eq::c -> (
    match s with
    | v1::v2::s -> (
      match zam_value_eq v1 v2 with
      | Some(b) -> run_zam_code_sub c env (ZAM_BoolVal(b)::s) r
      | None -> failwith "error at ZAM_Eq"
    )
    | _ -> failwith "error at ZAM_Eq"
  )
  | ZAM_NotEq::c -> (
    match s with
    | v1::v2::s -> (
      match zam_value_eq v1 v2 with
      | Some(b) -> run_zam_code_sub c env (ZAM_BoolVal(not b)::s) r
      | None -> failwith "error at ZAM_NotEq"
    )
    | _ -> failwith "error at ZAM_NotEq"
  )
  | ZAM_Less::c -> (
    match s with
    | ZAM_IntVal(n1)::ZAM_IntVal(n2)::s -> run_zam_code_sub c env (ZAM_BoolVal(n1 < n2)::s) r
    | _ -> failwith "error at ZAM_Less"
  )
  | ZAM_Greater::c -> (
    match s with
    | ZAM_IntVal(n1)::ZAM_IntVal(n2)::s -> run_zam_code_sub c env (ZAM_BoolVal(n1 > n2)::s) r
    | _ -> failwith "error at ZAM_Greater"
  )
  | ZAM_Not::c -> (
    match s with
    | ZAM_BoolVal(b)::s -> run_zam_code_sub c env (ZAM_BoolVal(not b)::s) r
    | _ -> failwith "error at ZAM_Not"
  )
  | ZAM_Cons::c -> (
    match s with
    | v::(ZAM_ListVal(l))::s -> run_zam_code_sub c env (ZAM_ListVal(v::l)::s) r
    | _ -> failwith "error at ZAM_Cons"
  )
  | ZAM_Head::c -> (
    match s with
    | (ZAM_ListVal(l))::s -> run_zam_code_sub c env ((List.hd l)::s) r
    | _ -> failwith "error at ZAM_Head"
  )
  | ZAM_Tail::c -> (
    match s with
    | (ZAM_ListVal(l))::s -> run_zam_code_sub c env (ZAM_ListVal(List.tl l)::s) r
    | _ -> failwith "error at ZAM_Tail"
  )
  | ZAM_ListLength::c -> (
    match s with
    | (ZAM_ListVal(l))::s -> run_zam_code_sub c env (ZAM_IntVal(List.length l)::s) r
    | _ -> failwith "error at ZAM_ListLength"
  )
  | ZAM_Raise::c -> run_zam_code_sub c env (ZAM_Fail("")::s) r
  | ZAM_FailWith(msg)::c -> run_zam_code_sub c env (ZAM_Fail(msg)::s) r
  | ZAM_Try(handler)::c -> (
    match s with
    | ZAM_Fail(_)::s -> run_zam_code_sub (handler@c) env s r (* handlerを実行 *)
    | _ -> run_zam_code_sub c env s r (* そのまま素通り *)
  )

  | ZAM_Apply :: c -> (
    match s with
    | ZAM_ClosVal(cl, envl)::v::s -> run_zam_code_sub cl (v::ZAM_ClosVal(cl,envl)::envl) s (ZAM_ClosVal(c, env)::r)
    | _ -> failwith "error at ZAM_Apply"
  )
  | ZAM_TailApply::_ -> (
    match s with
    | ZAM_ClosVal(cl, envl)::v::s -> run_zam_code_sub cl (v::ZAM_ClosVal(cl,envl)::envl) s r
    | _ -> failwith "error at ZAM_Apply"
  )
  | ZAM_PushMark::c -> run_zam_code_sub c env (ZAM_Epsilon::s) r
  | ZAM_Grab::c -> (
    match s with
    | ZAM_Epsilon::s -> (
      match r with
      | ZAM_ClosVal(cl, envl)::r -> run_zam_code_sub cl envl (ZAM_ClosVal(c, env)::s) r
      | _ -> failwith "error at ZAM_Grab"
    )
    | v::s -> run_zam_code_sub c (v::ZAM_ClosVal(c, env)::env) s r
    | _ -> failwith "error at ZAM_Grab"
  )
  | ZAM_Return::_ -> (
    match s with
    | v::ZAM_Epsilon::s -> (
      match r with
      | ZAM_ClosVal(cl, envl)::r -> run_zam_code_sub cl envl (v::s) r
      | _ -> failwith "error at ZAM_Return"
    )
    | v::ZAM_ClosVal(cl, envl)::s -> run_zam_code_sub cl (v::ZAM_ClosVal(cl, envl)::envl) s r
    | _ -> failwith "error at ZAM_Return"
  )
  | [] -> (
    match (s, env, r) with
    | ([value], [], []) -> value
    | _ -> failwith "error" (*スタックの要素が2つ以上あったり、 環境が空でなければ、エラーとする*)
  )

let run_zam_code zam_code = run_zam_code_sub zam_code [] [] []

let rec position (x : string) (venv : (string * exp option) list) : int * exp option =
  match venv with
    | [] -> failwith "no matching variable in environment"
    | (y,v)::venv2 -> (
      if x=y then
        (0, v)
      else
        let (n, opt) = (position x venv2) in
        (n + 1, opt)
    )

let rec search_fun (e: exp) (venv: (string * exp option) list): (string * exp) option =
  match e with
  | Var(x) -> (
    match position x venv with
    | (_, Some(Fun(name, e))) -> Some((name, e))
    | (_, Some(e2)) -> search_fun (e2) venv
    | _ -> None
  )
  | Fun(name, e) -> Some((name, e))
  | _ -> None

(* コンパイラ *)
let rec c_zam (e: exp) (venv: (string * exp option) list): zam_code =
  match e with
  | Var(x) -> (
    (* 環境を検索してxの値がリテラルであったときにその定数に置き換える *)
    match position x venv with
    | (_, Some(IntLit(n))) -> [ZAM_Ldi(n)]
    | (_, Some(BoolLit(b))) -> [ZAM_Ldb(b)]
    | (_, Some(StringLit(s))) -> [ZAM_Lds(s)]
    | (n, _) -> [ZAM_Access(n)]
  )
  | IntLit(n) -> [ZAM_Ldi(n)]
  | BoolLit(b) -> [ZAM_Ldb(b)]
  | StringLit(s) -> [ZAM_Lds(s)]
  | Plus(e1, e2) -> (
    match (c_zam e2 venv, c_zam e1 venv) with
    | ([ZAM_Ldi(n2)], [ZAM_Ldi(n1)]) -> [ZAM_Ldi(n1 + n2)]
    | (code2, code1) -> code2 @ code1 @ [ZAM_Add]
  )
  | Minus(e1, e2) -> (
    match (c_zam e2 venv, c_zam e1 venv) with
    | ([ZAM_Ldi(n2)], [ZAM_Ldi(n1)]) -> [ZAM_Ldi(n1 - n2)]
    | (code2, code1) -> code2 @ code1 @ [ZAM_Minus]
  )
  | Times(e1, e2) -> (
    match (c_zam e2 venv, c_zam e1 venv) with
    | ([ZAM_Ldi(n2)], [ZAM_Ldi(n1)]) -> [ZAM_Ldi(n1 * n2)]
    | (code2, code1) -> code2 @ code1 @ [ZAM_Times]
  )
  | Div(e1, e2) -> (
    match (c_zam e2 venv, c_zam e1 venv) with
    | ([ZAM_Ldi(n2)], [ZAM_Ldi(n1)]) -> [ZAM_Ldi(n1 / n2)]
    | (code2, code1) -> code2 @ code1 @ [ZAM_Div]
  )
  | Less(e1, e2) -> (
    match (c_zam e2 venv, c_zam e1 venv) with
    | ([ZAM_Ldi(n2)], [ZAM_Ldi(n1)]) -> [ZAM_Ldb(n1 < n2)]
    | (code2, code1) -> code2 @ code1 @ [ZAM_Less]
  )
  | Greater(e1, e2) -> (
    match (c_zam e2 venv, c_zam e1 venv) with
    | ([ZAM_Ldi(n2)], [ZAM_Ldi(n1)]) -> [ZAM_Ldb(n1 > n2)]
    | (code2, code1) -> code2 @ code1 @ [ZAM_Greater]
  )
  | Eq(e1, e2) -> (
    match (c_zam e2 venv, c_zam e1 venv) with
    | ([ZAM_Ldi(n2)], [ZAM_Ldi(n1)]) -> [ZAM_Ldb(n1 = n2)]
    | ([ZAM_Lds(s2)], [ZAM_Lds(s1)]) -> [ZAM_Ldb(s1 = s2)]
    | ([ZAM_Ldb(b2)], [ZAM_Ldb(b1)]) -> [ZAM_Ldb(b1 = b2)]
    | (code2, code1) -> code2 @ code1 @ [ZAM_Eq]
  )
  | NotEq(e1, e2) -> (
    match (c_zam e2 venv, c_zam e1 venv) with
    | ([ZAM_Ldi(n2)], [ZAM_Ldi(n1)]) -> [ZAM_Ldb(n1 <> n2)]
    | ([ZAM_Lds(s2)], [ZAM_Lds(s1)]) -> [ZAM_Ldb(s1 <> s2)]
    | ([ZAM_Ldb(b2)], [ZAM_Ldb(b1)]) -> [ZAM_Ldb(b1 <> b2)]
    | (code2, code1) -> code2 @ code1 @ [ZAM_NotEq]
  )
  | Not(e) -> (
    match c_zam e venv with
    | [ZAM_Ldb(b)] -> [ZAM_Ldb(not b)]
    | code -> code @ [ZAM_Not]
  )
  | If(e1, e2, e3) -> (
    match c_zam e1 venv with
    | [ZAM_Ldb(true)] -> c_zam e2 venv
    | [ZAM_Ldb(false)] -> c_zam e3 venv
    | code -> code @ [ZAM_Test(c_zam e2 venv, c_zam e3 venv)]
  )
  | Empty -> [ZAM_Ldl([])]
  | Cons(e1, e2) -> (
    match (c_zam e2 venv, c_zam e1 venv) with
    | ([ZAM_Ldb(b)], [ZAM_Ldl(l)]) -> [ZAM_Ldl([ZAM_Ldb(b)]::l)]
    | ([ZAM_Ldi(n)], [ZAM_Ldl(l)]) -> [ZAM_Ldl([ZAM_Ldi(n)]::l)]
    | ([ZAM_Lds(s)], [ZAM_Ldl(l)]) -> [ZAM_Ldl([ZAM_Lds(s)]::l)]
    | (code2, code1) -> code2 @ code1 @ [ZAM_Cons]
  )
  | Head(e) -> (
    match c_zam e venv with
    | ([ZAM_Ldl(l)]) -> List.hd l
    | code -> code @ [ZAM_Head]
  )
  | Tail(e) -> (
    match c_zam e venv with
    | ([ZAM_Ldl(l)]) -> [ZAM_Ldl(List.tl l)]
    | code -> code @ [ZAM_Tail]
  )
  | ListLength(e) -> (
    match c_zam e venv with
    | ([ZAM_Ldl(l)]) -> [ZAM_Ldi(List.length l)]
    | code -> code @ [ZAM_ListLength]
  )

  | Raise -> [ZAM_Raise]
  | FailWith(msg) -> [ZAM_FailWith(msg)]
  | Try(e, handler) -> c_zam e venv @ [ZAM_Try(c_zam handler venv)]

  | Fun(x, e1) -> [ZAM_Closure(t_zam e1 ((x, None)::("", None)::venv))]
  | Let(x, e1, e2) -> c_zam e1 venv @ [ZAM_Let] @ c_zam e2 ((x, Some(e1))::venv) @ [ZAM_EndLet]
  | LetRec(f, x, e1, e2) -> [ZAM_Closure(t_zam e1 ((x, None)::(f, None)::venv)); ZAM_Let] @ c_zam e2 ((f,None)::venv) @ [ZAM_EndLet]
  | App(e1, e2) -> (
    (* 関数適用の場合だけを展開したい *)
    let fun_opt = search_fun e1 venv in
    (* 引数が定数か変数の場合のみ *)
    let arg_value_opt =
      match e2 with
      | Var(_) | IntLit(_) | BoolLit(_) | StringLit(_) -> Some(e2)
      | _ -> None
    in
    match (fun_opt, arg_value_opt) with
    | (Some((arg_name, e)), Some(body_exp)) -> (
      let exp = replace arg_name body_exp e in
      c_zam exp venv
    )
    | _ -> ZAM_PushMark :: c_zam e2 venv @ c_zam e1 venv @ [ZAM_Apply]
  )
  | _ -> failwith "no supported"

and t_zam (e: exp) (venv: (string * exp option) list): zam_code =
  match e with
  | Var(x) -> (
    (* 環境を検索してxの値がリテラルであったときにその定数に置き換える *)
    match position x venv with
    | (_, Some(IntLit(n))) -> [ZAM_Ldi(n)]
    | (_, Some(BoolLit(b))) -> [ZAM_Ldb(b)]
    | (_, Some(StringLit(s))) -> [ZAM_Lds(s)]
    | (n, _) -> [ZAM_Access(n); ZAM_Return]
  )
  | IntLit(n) -> [ZAM_Ldi(n); ZAM_Return]
  | BoolLit(b) -> [ZAM_Ldb(b); ZAM_Return]
  | StringLit(b) -> [ZAM_Lds(b); ZAM_Return]
  | Plus(e1, e2) -> (
    match (c_zam e2 venv, c_zam e1 venv) with
    | ([ZAM_Ldi(n2)], [ZAM_Ldi(n1)]) -> [ZAM_Ldi(n1 + n2); ZAM_Return]
    | (code2, code1) -> code2 @ code1 @ [ZAM_Add; ZAM_Return]
  )
  | Minus(e1, e2) -> (
    match (c_zam e2 venv, c_zam e1 venv) with
    | ([ZAM_Ldi(n2)], [ZAM_Ldi(n1)]) -> [ZAM_Ldi(n1 - n2); ZAM_Return]
    | (code2, code1) -> code2 @ code1 @ [ZAM_Minus; ZAM_Return]
  )
  | Times(e1, e2) -> (
    match (c_zam e2 venv, c_zam e1 venv) with
    | ([ZAM_Ldi(n2)], [ZAM_Ldi(n1)]) -> [ZAM_Ldi(n1 * n2); ZAM_Return]
    | (code2, code1) -> code2 @ code1 @ [ZAM_Times; ZAM_Return]
  )
  | Div(e1, e2) -> (
    match (c_zam e2 venv, c_zam e1 venv) with
    | ([ZAM_Ldi(n2)], [ZAM_Ldi(n1)]) -> [ZAM_Ldi(n1 / n2); ZAM_Return]
    | (code2, code1) -> code2 @ code1 @ [ZAM_Div; ZAM_Return]
  )
  | Less(e1, e2) -> (
    match (c_zam e2 venv, c_zam e1 venv) with
    | ([ZAM_Ldi(n2)], [ZAM_Ldi(n1)]) -> [ZAM_Ldb(n1 < n2); ZAM_Return]
    | (code2, code1) -> code2 @ code1 @ [ZAM_Less; ZAM_Return]
  )
  | Greater(e1, e2) -> (
    match (c_zam e2 venv, c_zam e1 venv) with
    | ([ZAM_Ldi(n2)], [ZAM_Ldi(n1)]) -> [ZAM_Ldb(n1 > n2); ZAM_Return]
    | (code2, code1) -> code2 @ code1 @ [ZAM_Greater; ZAM_Return]
  )
  | Eq(e1, e2) -> (
    match (c_zam e2 venv, c_zam e1 venv) with
    | ([ZAM_Ldi(n2)], [ZAM_Ldi(n1)]) -> [ZAM_Ldb(n1 = n2)]
    | ([ZAM_Lds(s2)], [ZAM_Lds(s1)]) -> [ZAM_Ldb(s1 = s2)]
    | ([ZAM_Ldb(b2)], [ZAM_Ldb(b1)]) -> [ZAM_Ldb(b1 = b2)]
    | (code2, code1) -> code2 @ code1 @ [ZAM_Eq]
  )
  | NotEq(e1, e2) -> (
    match (c_zam e2 venv, c_zam e1 venv) with
    | ([ZAM_Ldi(n2)], [ZAM_Ldi(n1)]) -> [ZAM_Ldb(n1 <> n2); ZAM_Return]
    | ([ZAM_Lds(s2)], [ZAM_Lds(s1)]) -> [ZAM_Ldb(s1 <> s2); ZAM_Return]
    | ([ZAM_Ldb(b2)], [ZAM_Ldb(b1)]) -> [ZAM_Ldb(b1 <> b2); ZAM_Return]
    | (code2, code1) -> code2 @ code1 @ [ZAM_NotEq; ZAM_Return]
  )
  | Not(e) -> (
    match c_zam e venv with
    | [ZAM_Ldb(b)] -> [ZAM_Ldb(not b); ZAM_Return]
    | code -> code @ [ZAM_Not; ZAM_Return]
  )
  | If(e1, e2, e3) -> (
    match c_zam e1 venv with
    | [ZAM_Ldb(true)] -> c_zam e2 venv @ [ZAM_Return]
    | [ZAM_Ldb(false)] -> c_zam e3 venv @ [ZAM_Return]
    | code -> code @ [ZAM_Test(c_zam e2 venv, c_zam e3 venv); ZAM_Return]
  )
  | Empty -> [ZAM_Ldl([]); ZAM_Return]
  | Cons(e1, e2) -> (
    match (c_zam e2 venv, c_zam e1 venv) with
    | ([ZAM_Ldb(b)], [ZAM_Ldl(l)]) -> [ZAM_Ldl([ZAM_Ldb(b)]::l); ZAM_Return]
    | ([ZAM_Ldi(n)], [ZAM_Ldl(l)]) -> [ZAM_Ldl([ZAM_Ldi(n)]::l); ZAM_Return]
    | ([ZAM_Lds(s)], [ZAM_Ldl(l)]) -> [ZAM_Ldl([ZAM_Lds(s)]::l); ZAM_Return]
    | (code2, code1) -> code2 @ code1 @ [ZAM_Cons; ZAM_Return]
  )
  | Head(e) -> (
    match c_zam e venv with
    | ([ZAM_Ldl(l)]) -> List.hd l @ [ZAM_Return]
    | code -> code @ [ZAM_Head; ZAM_Return]
  )
  | Tail(e) -> (
    match c_zam e venv with
    | ([ZAM_Ldl(l)]) -> [ZAM_Ldl(List.tl l); ZAM_Return]
    | code -> code @ [ZAM_Tail; ZAM_Return]
  )
  | ListLength(e) -> (
    match c_zam e venv with
    | ([ZAM_Ldl(l)]) -> [ZAM_Ldi(List.length l); ZAM_Return]
    | code -> code @ [ZAM_ListLength; ZAM_Return]
  )


  | Raise -> [ZAM_Raise; ZAM_Return]
  | FailWith(msg) -> [ZAM_FailWith(msg); ZAM_Return]
  | Try(e, handler) -> c_zam e venv @ [ZAM_Try(c_zam handler venv); ZAM_Return]

  | Fun(x, e1) -> ZAM_Grab :: t_zam e1 ((x, None)::("", None)::venv)
  | Let(x, e1, e2) -> c_zam e1 venv @ [ZAM_Let] @ t_zam e2 ((x, Some(e1))::venv)
  | LetRec(f, x, e1, e2) -> [ZAM_Closure(t_zam e1 ((x, None)::(f, None)::venv)); ZAM_Let] @ t_zam e2 ((f, None)::venv)
  | App(e1, e2) ->  (
    (* 関数適用の場合だけを展開したい *)
    let fun_opt = search_fun e1 venv in
    (* 引数が定数か変数の場合のみ *)
    let arg_value_opt =
      match e2 with
      | Var(_) | IntLit(_) | BoolLit(_) | StringLit(_) -> Some(e2)
      | _ -> None
    in
    match (fun_opt, arg_value_opt) with
    | (Some((arg_name, e)), Some(body_exp)) -> (
      let exp = replace arg_name body_exp e in
      c_zam exp venv
    )
    | _ -> c_zam e2 venv @ c_zam e1 venv @ [ZAM_TailApply]
  )
  | _ -> failwith "no supported"


(* e内に含まれるarg_nameと同じ変数名の変数をarg_expに置き換える *)
and replace (arg_name: string) (arg_exp: exp) (e: exp): exp =
  match e with
  | Var(var) -> if var = arg_name then arg_exp else e
  | Plus(e1, e2) -> Plus(
      replace arg_name arg_exp e1,
      replace arg_name arg_exp e2
    )
  | Minus(e1, e2) -> Minus(
      replace arg_name arg_exp e1,
      replace arg_name arg_exp e2
    )
  | Times(e1, e2) -> Times(
      replace arg_name arg_exp e1,
      replace arg_name arg_exp e2
    )
  | Div(e1, e2) -> Div(
      replace arg_name arg_exp e1,
      replace arg_name arg_exp e2
    )
  | Eq(e1, e2) -> Eq(
      replace arg_name arg_exp e1,
      replace arg_name arg_exp e2
    )
  | NotEq(e1, e2) -> NotEq(
      replace arg_name arg_exp e1,
      replace arg_name arg_exp e2
    )
  | Not(e) -> Not(replace arg_name arg_exp e)
  | Less(e1, e2) -> Less(
      replace arg_name arg_exp e1,
      replace arg_name arg_exp e2
    )
  | Greater(e1, e2) -> Greater(
      replace arg_name arg_exp e1,
      replace arg_name arg_exp e2
    )
  | If(e1, e2, e3) -> If(
      replace arg_name arg_exp e1,
      replace arg_name arg_exp e2,
      replace arg_name arg_exp e3
    )
  | Cons(e1, e2) -> Cons(
    replace arg_name arg_exp e1,
    replace arg_name arg_exp e2
  )
  | Head(e) -> Head(replace arg_name arg_exp e)
  | Tail(e) -> Tail(replace arg_name arg_exp e)
  | ListLength(e) -> ListLength(replace arg_name arg_exp e)

  | App(e1, e2) -> App(
      replace arg_name arg_exp e1,
      replace arg_name arg_exp e2
    )
  | Fun(x, e1) -> (
    if x = arg_name then
      (* 変数名が上書きされるので何もしない *)
      Fun(x, e1)
    else
      Fun(x, replace arg_name arg_exp e1)
  )
  | Let(x, e1, e2) -> (
    if x = arg_name then
      Let(x, e1, e2)
    else
      Let(x, replace arg_name arg_exp e1, replace arg_name arg_exp e2)
  )
  | LetRec(f, x, e1, e2) -> (
    if f = arg_name || x = arg_name then
      LetRec(f, x, e1, e2)
    else
      LetRec(f, x, replace arg_name arg_exp e1, replace arg_name arg_exp e2)
  )
  | _ -> e
