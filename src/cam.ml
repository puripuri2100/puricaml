open Syntax
open Machine


let rec cam_value_eq v1 v2 =
  match (v1, v2) with
  | (CAM_IntVal(n1), CAM_IntVal(n2)) -> Some(n1 = n2)
  | (CAM_BoolVal(b1), CAM_BoolVal(b2)) -> Some(b1 = b2)
  | (CAM_StringVal(b1), CAM_StringVal(b2)) -> Some(b1 = b2)
  | (CAM_ListVal(l1), CAM_ListVal(l2)) -> (
    let rec f l1 l2 =
      match (l1, l2) with
      | (h1::tail1, h2::tail2) -> (
        match (cam_value_eq h1 h2, f tail1 tail2) with
        | (Some(b1), Some(b2)) -> Some(b1 && b2)
        | _ -> None
      )
      | ([], []) -> Some(true)
      | _ -> None
    in
    f l1 l2
  )
  | _ -> None

let rec run_cam_code_sub (code: cam_code) (env: cam_env) (s: cam_stack): cam_value =
  match code with
  | CAM_Ldi(n) :: c -> run_cam_code_sub c env (CAM_IntVal(n)::s)
  | CAM_Ldb(b) :: c -> run_cam_code_sub c env (CAM_BoolVal(b)::s)
  | CAM_Lds(str) :: c -> run_cam_code_sub c env (CAM_StringVal(str)::s)
  | CAM_Ldl(l) :: c -> (
    let l = List.map (fun c -> run_cam_code_sub c env s) l in
    run_cam_code_sub c env (CAM_ListVal(l)::s)
  )
  | CAM_Access(i) :: c -> run_cam_code_sub c env ((List.nth env i)::s)
  | CAM_Closure(cl) :: c -> run_cam_code_sub c env (CAM_ClosVal(cl, env)::s)
  | CAM_Apply :: c -> (
    match s with
    | CAM_ClosVal(cl, envl)::v::s -> run_cam_code_sub cl (v::CAM_ClosVal(cl,envl)::envl) (CAM_ClosVal(c, env)::s)
    | _ -> failwith "error at CAM_Apply"
  )
  | CAM_Return::_ -> (
    match s with
    | v::CAM_ClosVal(cl, envl)::s -> run_cam_code_sub cl envl (v::s)
    | _ -> failwith "error at CAM_Return"
  )
  | CAM_Let::c -> (
    match s with
    | v::s -> run_cam_code_sub c (v::env) s
    | _ -> failwith "error at CAM_Let"
  )
  | CAM_EndLet::c -> (
    match env with
    | _::env -> run_cam_code_sub c env s
    | _ -> failwith "error at CAM_EndLet"
  )
  | CAM_Test(c1, c2)::c -> (
    match s with
    | CAM_BoolVal(true) :: s -> run_cam_code_sub (c1@c) env s
    | CAM_BoolVal(false) :: s -> run_cam_code_sub (c2@c) env s
    | _ -> failwith "error at CAM_Test"
  )
  | CAM_Add::c -> (
    match s with
    | CAM_IntVal(n1)::CAM_IntVal(n2)::s -> run_cam_code_sub c env (CAM_IntVal(n1 + n2)::s)
    | _ -> failwith "error at CAM_Add"
  )
  | CAM_Minus::c -> (
    match s with
    | CAM_IntVal(n1)::CAM_IntVal(n2)::s -> run_cam_code_sub c env (CAM_IntVal(n1 - n2)::s)
    | _ -> failwith "error at CAM_Minus"
  )
  | CAM_Times::c -> (
    match s with
    | CAM_IntVal(n1)::CAM_IntVal(n2)::s -> run_cam_code_sub c env (CAM_IntVal(n1 * n2)::s)
    | _ -> failwith "error at CAM_Times"
  )
  | CAM_Div::c -> (
    match s with
    | CAM_IntVal(n1)::CAM_IntVal(n2)::s -> run_cam_code_sub c env (CAM_IntVal(n1 / n2)::s)
    | _ -> failwith "error at CAM_Div"
  )
  | CAM_Greater::c -> (
    match s with
    | CAM_IntVal(n1)::CAM_IntVal(n2)::s -> run_cam_code_sub c env (CAM_BoolVal(n1 > n2)::s)
    | _ -> failwith "error at CAM_Greater"
  )
  | CAM_Less::c -> (
    match s with
    | CAM_IntVal(n1)::CAM_IntVal(n2)::s -> run_cam_code_sub c env (CAM_BoolVal(n1 < n2)::s)
    | _ -> failwith "error at CAM_Less"
  )
  | CAM_Eq::c -> (
    match s with
    | v1::v2::s -> (
      match cam_value_eq v1 v2 with
      | Some(b) -> run_cam_code_sub c env (CAM_BoolVal(b)::s)
      | None -> failwith "error at CAM_Eq"
    )
    | _ -> failwith "error at CAM_Eq"
  )
  | CAM_NotEq::c -> (
    match s with
    | v1::v2::s -> (
      match cam_value_eq v1 v2 with
      | Some(b) -> run_cam_code_sub c env (CAM_BoolVal(b)::s)
      | None -> failwith "error at CAM_NotEq"
    )
    | _ -> failwith "error at CAM_NotEq"
  )
  | CAM_Not::c -> (
    match s with
    | CAM_BoolVal(b)::s -> run_cam_code_sub c env (CAM_BoolVal(not b)::s)
    | _ -> failwith "error at CAM_Not"
  )
  | CAM_Cons::c -> (
    match s with
    | v::(CAM_ListVal(l))::s -> run_cam_code_sub c env (CAM_ListVal(v::l)::s)
    | _ -> failwith "error at CAM_Cons"
  )
  | CAM_Head::c -> (
    match s with
    | (CAM_ListVal(l))::s -> run_cam_code_sub c env ((List.hd l)::s)
    | _ -> failwith "error at CAM_Head"
  )
  | CAM_Tail::c -> (
    match s with
    | (CAM_ListVal(l))::s -> run_cam_code_sub c env (CAM_ListVal(List.tl l)::s)
    | _ -> failwith "error at CAM_Tail"
  )
  | CAM_ListLength::c -> (
    match s with
    | (CAM_ListVal(l))::s -> run_cam_code_sub c env (CAM_IntVal(List.length l)::s)
    | _ -> failwith "error at CAM_ListLength"
  )
  | CAM_Raise::c -> run_cam_code_sub c env (CAM_Fail("")::s)
  | CAM_FailWith(msg)::c -> run_cam_code_sub c env (CAM_Fail(msg)::s)
  | CAM_Try(handler)::c -> (
    match s with
    | CAM_Fail(_)::s -> run_cam_code_sub (handler@c) env s (* handlerを実行 *)
    | _ -> run_cam_code_sub c env s (* そのまま素通り *)
  )
  | [] -> (
    match (s, env) with
    | ([value], []) -> value
    | _ -> let _ = Printf.printf "s length: %d,\n env length: %d\n" (List.length s) (List.length env) in failwith "error env" (*スタックの要素が2つ以上あったり、 環境が空でなければ、エラーとする*)
  )


let run_cam_code cam_code = run_cam_code_sub cam_code [] []

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
let rec c_cam (e: exp) (venv: (string * exp option) list): cam_code =
  match e with
  | Var(x) -> (
    (* 環境を検索してxの値がリテラルであったときにその定数に置き換える *)
    match position x venv with
    | (_, Some(IntLit(n))) -> [CAM_Ldi(n)]
    | (_, Some(BoolLit(b))) -> [CAM_Ldb(b)]
    | (_, Some(StringLit(b))) -> [CAM_Lds(b)]
    | (n, _) -> [CAM_Access(n)]
  )
  | Fun(x, e1) -> [CAM_Closure(c_cam e1 ((x, None)::("", None)::venv) @ [CAM_Return])]
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
      c_cam exp venv
    )
    | _ -> c_cam e2 venv @ c_cam e1 venv @ [CAM_Apply]
  )
  | Let(x, e1, e2) -> c_cam e1 venv @ [CAM_Let] @ c_cam e2 ((x, Some(e1))::venv) @ [CAM_EndLet]
  | LetRec(f, x, e1, e2) -> [CAM_Closure(c_cam e1 ((x, None)::(f, None)::venv) @ [CAM_Return]); CAM_Let] @ c_cam e2 ((f, None)::venv) @ [CAM_EndLet]
  | IntLit(n) -> [CAM_Ldi(n)]
  | BoolLit(b) -> [CAM_Ldb(b)]
  | StringLit(s) -> [CAM_Lds(s)]
  | Plus(e1, e2) -> (
    match (c_cam e2 venv, c_cam e1 venv) with
    | ([CAM_Ldi(n2)], [CAM_Ldi(n1)]) -> [CAM_Ldi(n1 + n2)]
    | (code2, code1) -> code2 @ code1 @ [CAM_Add]
  )
  | Minus(e1, e2) -> (
    match (c_cam e2 venv, c_cam e1 venv) with
    | ([CAM_Ldi(n2)], [CAM_Ldi(n1)]) -> [CAM_Ldi(n1 - n2)]
    | (code2, code1) -> code2 @ code1 @ [CAM_Minus]
  )
  | Times(e1, e2) -> (
    match (c_cam e2 venv, c_cam e1 venv) with
    | ([CAM_Ldi(n2)], [CAM_Ldi(n1)]) -> [CAM_Ldi(n1 * n2)]
    | (code2, code1) -> code2 @ code1 @ [CAM_Times]
  )
  | Div(e1, e2) -> (
    match (c_cam e2 venv, c_cam e1 venv) with
    | ([CAM_Ldi(n2)], [CAM_Ldi(n1)]) -> [CAM_Ldi(n1 / n2)]
    | (code2, code1) -> code2 @ code1 @ [CAM_Div]
  )
  | Less(e1, e2) -> (
    match (c_cam e2 venv, c_cam e1 venv) with
    | ([CAM_Ldi(n2)], [CAM_Ldi(n1)]) -> [CAM_Ldb(n1 < n2)]
    | (code2, code1) -> code2 @ code1 @ [CAM_Less]
  )
  | Greater(e1, e2) -> (
    match (c_cam e2 venv, c_cam e1 venv) with
    | ([CAM_Ldi(n2)], [CAM_Ldi(n1)]) -> [CAM_Ldb(n1 > n2)]
    | (code2, code1) -> code2 @ code1 @ [CAM_Greater]
  )
  | Eq(e1, e2) -> (
    match (c_cam e2 venv, c_cam e1 venv) with
    | ([CAM_Ldi(n2)], [CAM_Ldi(n1)]) -> [CAM_Ldb(n1 = n2)]
    | ([CAM_Lds(s2)], [CAM_Lds(s1)]) -> [CAM_Ldb(s1 = s2)]
    | ([CAM_Ldb(b2)], [CAM_Ldb(b1)]) -> [CAM_Ldb(b1 = b2)]
    | (code2, code1) -> code2 @ code1 @ [CAM_Eq]
  )
  | NotEq(e1, e2) -> (
    match (c_cam e2 venv, c_cam e1 venv) with
    | ([CAM_Ldi(n2)], [CAM_Ldi(n1)]) -> [CAM_Ldb(n1 <> n2)]
    | ([CAM_Lds(s2)], [CAM_Lds(s1)]) -> [CAM_Ldb(s1 <> s2)]
    | ([CAM_Ldb(b2)], [CAM_Ldb(b1)]) -> [CAM_Ldb(b1 <> b2)]
    | (code2, code1) -> code2 @ code1 @ [CAM_NotEq]
  )
  | Not(e) -> (
    match c_cam e venv with
    | [CAM_Ldb(b)] -> [CAM_Ldb(not b)]
    | code -> code @ [CAM_Not]
  )
  | If(e1, e2, e3) -> (
    match c_cam e1 venv with
    | [CAM_Ldb(true)] -> c_cam e2 venv
    | [CAM_Ldb(false)] -> c_cam e3 venv
    | code -> code @ [CAM_Test(c_cam e2 venv, c_cam e3 venv)]
  )
  | Empty -> [CAM_Ldl([])]
  | Cons(e1, e2) -> (
    match (c_cam e2 venv, c_cam e1 venv) with
    | ([CAM_Ldb(b)], [CAM_Ldl(l)]) -> [CAM_Ldl([CAM_Ldb(b)]::l)]
    | ([CAM_Ldi(n)], [CAM_Ldl(l)]) -> [CAM_Ldl([CAM_Ldi(n)]::l)]
    | ([CAM_Lds(s)], [CAM_Ldl(l)]) -> [CAM_Ldl([CAM_Lds(s)]::l)]
    | (code2, code1) -> code2 @ code1 @ [CAM_Cons]
  )
  | Head(e) -> (
    match c_cam e venv with
    | ([CAM_Ldl(l)]) -> List.hd l
    | code -> code @ [CAM_Head]
  )
  | Tail(e) -> (
    match c_cam e venv with
    | ([CAM_Ldl(l)]) -> [CAM_Ldl(List.tl l)]
    | code -> code @ [CAM_Tail]
  )
  | ListLength(e) -> (
    match c_cam e venv with
    | ([CAM_Ldl(l)]) -> [CAM_Ldi(List.length l)]
    | code -> code @ [CAM_ListLength]
  )
  | Raise -> [CAM_Raise]
  | FailWith(msg) -> [CAM_FailWith(msg)]
  | Try(e, handler) -> c_cam e venv @ [CAM_Try(c_cam handler venv)]
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
