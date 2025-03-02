open Syntax

let emptyenv () = []

let ext env x v = (x, v) :: env

let rec lookup x env =
  match env with
  | [] -> failwith ("unbound variable: " ^ x)
  | (y, v) :: tl -> if x = y then v else lookup x tl

let rec value_eq (v1: value) (v2: value): bool option =
  match (v1, v2) with
  | (IntVal(n1), IntVal(n2)) -> Some(n1 = n2)
  | (BoolVal(b1), BoolVal(b2)) -> Some(b1 = b2)
  | (StringVal(b1), StringVal(b2)) -> Some(b1 = b2)
  | (ListVal(l1), ListVal(l2)) -> (
    let rec f l1 l2 =
      match (l1, l2) with
      | (h1::tail1, h2::tail2) -> (
        match (value_eq h1 h2, f tail1 tail2) with
        | (Some(b1), Some(b2)) -> Some(b1 && b2)
        | _ -> None
      )
      | ([], []) -> Some(true)
      | _ -> None
    in
    f l1 l2
  )
  | _ -> None

let rec eval e env ok err =
  let binop f e1 e2 env ok err =
    eval e1 env (fun v1 -> (
      eval e2 env (fun v2 -> (
        match (v1, v2) with
        | (IntVal(n1), IntVal(n2)) -> ok (IntVal(f n1 n2))
        | _ -> err (Fail("integer values expected"))
      )) err
    )) err
  in
  match e with
  | Var(x) -> ok (lookup x env)
  | IntLit(n) -> ok (IntVal(n))
  | BoolLit(b) -> ok (BoolVal(b))
  | StringLit(s) -> ok (StringVal(s))
  | NotEq(e1, e2) -> (
    eval e1 env (fun v1 -> (
      eval e2 env (fun v2 -> (
        match value_eq v1 v2 with
        | Some(b) -> ok (BoolVal(not b))
        | None -> err (Fail("No comparison"))
      )) err
    )) err
  )
  | Eq(e1, e2) -> (
    eval e1 env (fun v1 -> (
      eval e2 env (fun v2 -> (
        match value_eq v1 v2 with
        | Some(b) -> ok (BoolVal(b))
        | None -> err (Fail("No comparison"))
      )) err
    )) err
  )
  | Not(e) -> (
    eval e env (fun v1 -> (
      match v1 with
      | BoolVal(b) -> ok (BoolVal(not b))
      | _ -> err (Fail("boolean values expected"))
    )) err
  )
  | If(e1, e2, e3) -> (
    eval e1 env (fun v1 -> (
      match v1 with
      | BoolVal(true) -> eval e2 env ok err
      | BoolVal(false) -> eval e3 env ok err
      | _ -> err (Fail("wrong value"))
    )) err
  )
  | Let(x, e1, e2) -> (
    eval e1 env (fun v1 -> (
      let env1 = ext env x v1 in
      eval e2 env1 ok err
    )) err
  )
  | LetRec(f, x, e1, e2) -> (
    let env1 = ext env f (RecFunVal (f, x, e1, env)) in
    eval e2 env1 ok err
  )
  | Fun(x, e) -> ok (FunVal(x, e, env))
  | App(e1, e2) -> (
    eval e1 env (fun funpart -> (
      eval e2 env (fun arg -> (
        app funpart arg ok err
      )) err
    )) err
  )
  | Callcc(e1) -> (
    eval e1 env (fun funpart -> app funpart (ContVal ok) ok err) err
  )
  | Greater(e1, e2) -> (
    eval e1 env (fun v1 -> (
      eval e2 env (fun v2 -> (
        match (v1, v2) with
        | (IntVal(n1), IntVal(n2)) -> ok (BoolVal(n1 > n2))
        | _ -> err (Fail("integer values expected"))
      )) err
    )) err
  )
  | Less(e1, e2) -> (
    eval e1 env (fun v1 -> (
      eval e2 env (fun v2 -> (
        match (v1, v2) with
        | (IntVal(n1), IntVal(n2)) -> ok (BoolVal(n1 < n2))
        | _ -> err (Fail("integer values expected"))
      )) err
    )) err
  )
    | Plus(e1,e2) -> binop (+) e1 e2 env ok err
    | Times(e1,e2) -> binop ( * ) e1 e2 env ok err
    | Minus(e1, e2) -> binop (-) e1 e2 env ok err
    | Div(e1, e2) ->  binop (fun n1 n2 -> if n2 = 0 then failwith "div 0" else n1 / n2) e1 e2 env ok err
  | Empty -> ok (ListVal([]))
  | Cons(e1,e2) -> (
    eval e1 env (fun v1 -> (
      eval e2 env (fun v2 -> (
        match (v1, v2) with
        | (_, ListVal(v2)) -> ok (ListVal(v1 :: v2))
        | _ -> err (Fail("list values expected"))
      )) err
    )) err
  )
  | Head(e) -> (
    eval e env (fun v -> (
      match v with
      | ListVal(x::_) -> ok x
      | _ -> err (Fail("list values expected"))
    )) err
  )
  | Tail(e) -> (
    eval e env (fun v -> (
      match v with
      | ListVal(_::lst) -> ok (ListVal(lst))
      | _ -> err (Fail("list values expected"))
    )) err
  )
  | ListLength(e) -> (
    eval e env (fun v -> (
      match v with
      | ListVal(l) -> ok (IntVal(List.length l))
      | _ -> err (Fail("list values expected"))
    )) err
  )
  | Raise -> err (Fail(""))
  | FailWith(msg) -> err (Fail(msg))
  | Try(e, handler) -> eval e env ok (fun _ -> eval handler env ok err)

and app funpart arg ok err =
  match funpart with
  | FunVal(x,body,env1) ->
      eval body (ext env1 x arg) ok err
  | RecFunVal(f,x,body,env1) ->
      let env2 = (ext (ext env1 x arg) f funpart) in
        eval body env2 ok err
  | ContVal(cont1) -> cont1 arg
  | _ -> err (Fail "function value expected")
