open Syntax

let emptyenv () = []

let ext env x v = (x, v) :: env

let rec lookup x env =
  match env with
  | [] -> failwith ("unbound variable: " ^ x)
  | (y, v) :: tl -> if x = y then v else lookup x tl

  type tyvar = string

  type ty =
    | TInt
    | TBool
    | TString
    | TArrow of ty * ty
    | TVar of tyvar
    | TList of ty

  type tyenv = (string * ty) list
  
  type tysubst = (tyvar * ty) list

(* 型変数txがtの定義に含まれていないかを検査する *)
let rec occurs tx t =
  if tx = t then
    true 
  else 
    match t with
    | TArrow(t1,t2) -> (occurs tx t1) || (occurs tx t2)
    | _ -> false

(* subst_ty : tysubst -> ty -> ty *)
(* 代入thetaを型t に適用する *)
let rec subst_ty theta t =
  let rec subst_ty1 theta1 s = 
    match theta1 with
      | [] -> TVar(s)
      | (tx,t1):: theta2 ->
        if tx = s then
          t1 
        else
          subst_ty1 theta2 s
  in
  match t with
    | TInt -> TInt
    | TBool -> TBool
    | TString -> TString
    | TArrow(t2,t3) -> TArrow(subst_ty theta t2, subst_ty theta t3)
    | TVar(s) -> subst_ty1 theta s
    | TList(t) -> TList(subst_ty theta t)

(* subst_tyenv  : tysubst -> tyenv -> tyenv *)
(* 代入thetaを型環境 te に適用する *)
let subst_tyenv theta te =
  te
  |> List.map (fun (x,t) -> (x, subst_ty theta t))


(* subst_eq : tysubst -> (ty * ty) list -> (ty * ty) list *)
(* 代入thetaを型の等式のリスト eql に適用する *)
let subst_eql theta eql =
  eql
  |> List.map (fun (t1,t2) -> (subst_ty theta t1, subst_ty theta t2))

(* compose_subst : tysubst -> tysubst -> tysubst *)
(* 2つの代入を合成した代入を返す。theta1 が「先」でtheta2が「後」である *)
let compose_subst theta2 theta1 =
  let theta11 =
    List.map (fun (tx,t) -> (tx, subst_ty theta2 t)) theta1
  in
  List.fold_left (
    fun tau -> (
      fun (tx,t) -> (
        try 
          let _ = lookup tx theta1 in
          tau
        with Failure(_) ->
          (tx,t) :: tau
      )
    )
  ) theta11 theta2

let unify (eql: (ty * ty) list): tysubst =
  let rec solve eql theta =
    match eql with
    | [] -> theta
    | (t1,t2):: eql2 -> (
      if t1 = t2 then
        solve eql2 theta
      else 
        begin
          match (t1,t2) with
          | (TArrow(t11,t12),TArrow(t21,t22)) -> solve ((t11,t21)::(t12,t22)::eql2) theta
          | (TList(t1), TList(t2)) -> solve((t1, t2)::eql2) theta
          | (TVar(s), _) ->
            if (occurs t1 t2) then
              failwith "TypeError: unification failed"
            else
              solve (subst_eql [(s,t2)] eql2) (compose_subst [(s,t2)] theta)
          | (_,TVar(s)) ->
            if (occurs t2 t1) then
              failwith "TypeError: unification failed"
            else
              solve (subst_eql [(s,t1)] eql2) (compose_subst [(s,t1)] theta)
          | (_,_) -> failwith "TypeError: unification failed"
        end
    )
  in
  solve eql []

let rec remove te s =
  match te with
  | [] -> []
  | (s1, t)::tail ->
    if s1 = s then
      tail
    else
      (s1, t) :: remove tail s

let theta0 = ([] : tysubst)

(* new_typevar : int -> ty * int *)
let new_typevar n = 
  (TVar ("'a" ^ (string_of_int n)), n+1)

(* tinf2 : tyenv -> exp -> int -> tyenv * ty * tysubst * int *)
let rec tinf2 te e n =
  match e with
    | Var(s) -> (
      try
        let t1 = lookup s te in (te, t1, theta0, n)
      with Failure(_) ->
        let (tx,n1) = new_typevar n in
        let te1 = ext te s tx in
        (te1, tx, theta0, n1)
    )
    | IntLit(_)   -> (te, TInt, theta0, n)
    | BoolLit(_)  -> (te, TBool, theta0, n)
    | StringLit(_)-> (te, TString, theta0, n)
    | Plus(e1,e2) -> (
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
      let t11 = subst_ty theta2 t1 in
      let theta3 = unify [(t11,TInt); (t2,TInt)] in
      let te3 = subst_tyenv theta3 te2 in
      let theta4 = compose_subst theta3 (compose_subst theta2 theta1) in
      (te3, TInt, theta4, n2)
    )
    | Minus(e1,e2) -> (
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
      let t11 = subst_ty theta2 t1 in
      let theta3 = unify [(t11,TInt); (t2,TInt)] in
      let te3 = subst_tyenv theta3 te2 in
      let theta4 = compose_subst theta3 (compose_subst theta2 theta1) in
      (te3, TInt, theta4, n2)
    )
    | Times(e1,e2) -> (
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
      let t11 = subst_ty theta2 t1 in
      let theta3 = unify [(t11,TInt); (t2,TInt)] in
      let te3 = subst_tyenv theta3 te2 in
      let theta4 = compose_subst theta3 (compose_subst theta2 theta1) in
      (te3, TInt, theta4, n2)
    )
    | Div(e1,e2) -> (
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
      let t11 = subst_ty theta2 t1 in
      let theta3 = unify [(t11,TInt); (t2,TInt)] in
      let te3 = subst_tyenv theta3 te2 in
      let theta4 = compose_subst theta3 (compose_subst theta2 theta1) in
      (te3, TInt, theta4, n2)
    )
    | Less(e1,e2) -> (
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
      let t11 = subst_ty theta2 t1 in
      let theta3 = unify [(t11,TInt); (t2,TInt)] in
      let te3 = subst_tyenv theta3 te2 in
      let theta4 = compose_subst theta3 (compose_subst theta2 theta1) in
      (te3, TBool, theta4, n2)
    )
    | Greater(e1,e2) -> (
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
      let t11 = subst_ty theta2 t1 in
      let theta3 = unify [(t11,TInt); (t2,TInt)] in
      let te3 = subst_tyenv theta3 te2 in
      let theta4 = compose_subst theta3 (compose_subst theta2 theta1) in
      (te3, TBool, theta4, n2)
    )
    | Eq(e1,e2) -> (
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
      let t11 = subst_ty theta2 t1 in
      let theta3 = unify [(t11,t2)] in
      let te3 = subst_tyenv theta3 te2 in
      let theta4 = compose_subst theta3 (compose_subst theta2 theta1) in
      (te3, TBool, theta4, n2)
    )
    | NotEq(e1,e2) -> (
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
      let t11 = subst_ty theta2 t1 in
      let theta3 = unify [(t11,t2)] in
      let te3 = subst_tyenv theta3 te2 in
      let theta4 = compose_subst theta3 (compose_subst theta2 theta1) in
      (te3, TBool, theta4, n2)
    )
    | Not(e) -> (
      let (te1, t1, theta1, n1) = tinf2 te e n in
      let t11 = subst_ty theta1 t1 in
      let theta2 = unify [(t11,TBool)] in
      let te2 = subst_tyenv theta2 te1 in
      let theta3 = compose_subst theta2 theta1 in
      (te2, TBool, theta3, n1)
    )
    | If(e1,e2,e3) -> (
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
      let (te3, t3, theta3, n3) = tinf2 te2 e3 n2 in
      let t11 = subst_ty theta3 t1 in
      let t21 = subst_ty theta3 t2 in
      let theta4 = unify [(t11, TBool); (t21, t3)] in
      let te4 = subst_tyenv theta4 te3 in
      let theta5 =
        theta1
        |> compose_subst theta2
        |> compose_subst theta3
        |> compose_subst theta4
      in
      let t4 = subst_ty theta5 t3 in
      (te4, t4, theta5, n3)
    )
    | Fun(x,e) -> (
      let (tx,n1) = new_typevar n in
      let te1 = ext te x tx in
      let (te2, t1, theta1, n2) = tinf2 te1 e n1 in
      let t2 = subst_ty theta1 tx in
      let te3 = remove te2 x in
      (te3, TArrow(t2, t1), theta1, n2)
    )
    | App(e1,e2) -> (
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
      let (tx,n3) = new_typevar n2 in
      let t11 = subst_ty theta2 t1 in
      let theta3 = unify [(t11,TArrow(t2,tx))] in
      let t3 = subst_ty theta3 tx in
      let te3 = subst_tyenv theta3 te2 in
      let theta4 = compose_subst theta3 (compose_subst theta2 theta1) in
      (te3, t3, theta4, n3)
    )
    | Empty -> (
      let (tx, n1) = new_typevar n in
      (te, TList(tx), theta0, n1)
    )
    | Cons(e1, e2) -> (
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
      let t11 = subst_ty theta2 t1 in
      let theta3 = unify [(TList(t11),t2)] in
      let te3 = subst_tyenv theta3 te2 in
      let theta4 = compose_subst theta3 (compose_subst theta2 theta1) in
      let t3 = subst_ty theta4 t2 in
      (te3, t3, theta4, n2)
    )
    | Head(e) -> (
      let (te1, t1, theta1, n1) = tinf2 te e n in
      match t1 with
      | TList(t1) -> (te1, t1, theta1, n1)
      | _ -> failwith "TypeError: tinfo2 failed"
    )
    | Tail(e) -> (
      let (te1, t1, theta1, n1) = tinf2 te e n in
      let (tx, n2) = new_typevar n1 in
      let theta2 = unify [(t1, TList(tx))] in
      let te2 = subst_tyenv theta2 te1 in
      let theta3 = compose_subst theta2 theta1 in
      let t2 = subst_ty theta3 t1 in
      (te2, t2, theta3, n2)
    )
    | ListLength(e) -> (
      let (te1, t1, theta1, n1) = tinf2 te e n in
      let (tx, n2) = new_typevar n1 in
      let theta2 = unify [(t1, TList(tx))] in
      let te2 = subst_tyenv theta2 te1 in
      let theta3 = compose_subst theta2 theta1 in
      (te2, TInt, theta3, n2)
    )
    | Let(x, e1, e2) -> (
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let t11 = subst_ty theta1 t1 in
      let te2 = ext te1 x t11 in
      tinf2 te2 e2 n1
    )
    | LetRec(f, x, e1, e2) -> (
      (* 引数の型と返り値の型を仮に生成し、 f: tx -> tr として登録する *)
      let (tx, n1) = new_typevar n in
      let (tr, n2) = new_typevar n1 in
      let tf = TArrow(tx, tr) in
      let te1 = ext te f tf in
      let te2 = ext te1 x tx in

      (* fの返り値を推論し、trと一致するか検証 *)
      let (te3, t1, theta1, n3) = tinf2 te2 e1 n2 in
      let theta2 = unify [(subst_ty theta1 tr, t1)] in
      let te4 = subst_tyenv theta2 te3 in

      let te5 = remove te4 x in

      tinf2 te5 e2 n3
    )
    | Raise -> (
      (* 'a型を生成する *)
      let (tx, n1) = new_typevar n in
      (te, tx, theta0, n1)
    )
    | FailWith(_) -> (
      (* 'a型を生成する *)
      let (tx, n1) = new_typevar n in
      (te, tx, theta0, n1)
    )
    | Try(e, handler) -> (
      (* 式とハンドリング用の式の型が一致していることが必要 *)
      let (te1, t1, theta1, n1) = tinf2 te e n in
      let (te2, t2, theta2, n2) = tinf2 te1 handler n1 in
      let t11 = subst_ty theta2 t1 in
      let theta3 = unify [(t11,t2)] in
      let te3 = subst_tyenv theta3 te2 in
      let theta4 = compose_subst theta3 (compose_subst theta2 theta1) in
      let t3 = subst_ty theta4 t2 in
      (te3, t3, theta4, n2)
    )
    | _ -> failwith "TypeError: unknown expression"

let tinf2top e = tinf2 [] e 0

