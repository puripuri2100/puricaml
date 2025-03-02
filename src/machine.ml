(* ZAM *)
type zam_value =  
  | ZAM_IntVal  of int   (* ZAM の値に対するタグにはZAM_ をつける *)
  | ZAM_BoolVal of bool
  | ZAM_StringVal of string
  | ZAM_ListVal of zam_value list
  | ZAM_Fail of string (* エラーの伝播 *)
  | ZAM_ClosVal of zam_code * zam_env  (* 再帰関数に対応するクロージャ *)
  | ZAM_Epsilon  (* 渡されたすべての引数を使い切ったことを表す値ε *)
and zam_stack = zam_value list (* スタック *)
and zam_env = zam_value list (* 環境は、1つのスタックフレームに相当する。 *)

and zam_instr =
  | ZAM_Ldi of int                    (* CAM_Ldiと同じ *)
  | ZAM_Ldb of bool                   (* CAM_Ldbと同じ *)
  | ZAM_Lds of string
  | ZAM_Ldl of zam_code list
  | ZAM_Access of int                 (* CAM_Accessと同じ *)
  | ZAM_Closure of zam_code           (* CAM_Closureと同じ *)
  | ZAM_Let                           (* CAM_Letと同じ *)
  | ZAM_EndLet                        (* CAM_EndLetと同じ *)
  | ZAM_Test of zam_code * zam_code   (* CAM_Testと同じ *)
  | ZAM_Add                           (* CAM_Addと同じ *)
  | ZAM_Minus                         (* CAM_Minusと同じ *)
  | ZAM_Times                         (* CAM_Timesと同じ *)
  | ZAM_Div                           (* CAM_Divと同じ *)
  | ZAM_Eq                            (* CAM_Eqと同じ *)
  | ZAM_Less
  | ZAM_Greater
  | ZAM_NotEq
  | ZAM_Not
  | ZAM_Cons
  | ZAM_Head
  | ZAM_Tail
  | ZAM_ListLength

  | ZAM_Raise
  | ZAM_FailWith of string
  | ZAM_Try of zam_code

  | ZAM_Apply                         (* 関数呼び出し *)
  | ZAM_TailApply                     (* 末尾呼び出し *)
  | ZAM_PushMark                      (* 引数スタックに特殊な値εを積む *)
  | ZAM_Grab                          (* 引数スタックトップの値を環境に移す *)
  | ZAM_Return                        (* 関数呼び出し元に戻る *)
and zam_code = zam_instr list  (* コードは、命令の列である *)

let rec print_zam_value zam_value =
  match zam_value with
  | ZAM_IntVal(n) -> Printf.sprintf "ZAM_IntVal(%d)" n
  | ZAM_BoolVal(true) -> "ZAM_BoolVal(true)"
  | ZAM_BoolVal(false) -> "ZAM_BoolVal(false)"
  | ZAM_StringVal(s) -> Printf.sprintf "ZAM_StringVal(%s)" s
  | ZAM_ListVal(l) -> (
    let str =
      l
      |> List.map (fun v -> print_zam_value v)
      |> List.fold_left (fun s1 s2 -> s1 ^ s2 ^ ";") ""
    in
    Printf.sprintf "ZAM_ListVal([%s])" str 
  )
  | ZAM_Fail(msg) -> Printf.sprintf "ZAM_Fail(%s)" msg
  | ZAM_ClosVal(_) -> "ZAM_ClosVal"
  | ZAM_Epsilon -> "ZAM_Epsilon"


let rec print_zam_instr zam_instr =
  match zam_instr with
  | ZAM_Ldi(n) -> Printf.sprintf "Ldi %d" n
  | ZAM_Ldb(true) -> "Ldb true"
  | ZAM_Ldb(false) -> "Ldb false"
  | ZAM_Lds(s) -> Printf.sprintf "Lds \"%s\"" s
  | ZAM_Ldl(l) -> (
    let s =
      l
      |> List.map (fun code -> (
        code
        |> List.map print_zam_instr
        |> List.fold_left (fun s1 s2 -> s1 ^ s2 ^ ";") ""
      ))
      |> List.fold_left (fun s1 s2 -> s1 ^ "(" ^ s2 ^ "); ") ""
    in
    Printf.sprintf "Ldl [%s]" s
  )
  | ZAM_Access(n) -> Printf.sprintf "Access %d" n
  | ZAM_Closure(code) -> (
    let s =
      code
      |> List.map print_zam_instr
      |> List.fold_left (fun s1 s2 -> s1 ^ s2 ^ ";") ""
    in
    Printf.sprintf "Closure (%s)" s
  )
  | ZAM_Let -> "Let"
  | ZAM_EndLet -> "EndLet"
  | ZAM_Test(code1, code2) -> (
    let s1 =
      code1
      |> List.map print_zam_instr
      |> List.fold_left (fun s1 s2 -> s1 ^ s2 ^ ";") ""
    in
    let s2 =
      code2
      |> List.map print_zam_instr
      |> List.fold_left (fun s1 s2 -> s1 ^ s2 ^ ";") ""
    in
    Printf.sprintf "Test (%s, %s)" s1 s2
  )
  | ZAM_Add -> "Add"
  | ZAM_Minus -> "Minus"
  | ZAM_Times -> "Times"
  | ZAM_Div -> "Div"
  | ZAM_Eq -> "Eq"
  | ZAM_Less -> "Less"
  | ZAM_Greater -> "Greater"
  | ZAM_NotEq -> "NotEq"
  | ZAM_Not -> "Not"
  | ZAM_Cons -> "Cons"
  | ZAM_Head -> "Head"
  | ZAM_Tail -> "Tail"
  | ZAM_ListLength -> "ListLength"

  | ZAM_Raise -> "Raise"
  | ZAM_FailWith(s) -> Printf.sprintf "FailWith %s" s
  | ZAM_Try(code) -> (
    let s =
      code
      |> List.map print_zam_instr
      |> List.fold_left (fun s1 s2 -> s1 ^ s2 ^ ";") ""
    in
    Printf.sprintf "Try (%s)" s
  )
  | ZAM_Apply -> "Apply"
  | ZAM_TailApply -> "TailApply"
  | ZAM_PushMark -> "PushMark"
  | ZAM_Grab -> "Grab"
  | ZAM_Return -> "Return"

(* CAM *)
type cam_value =  
  | CAM_IntVal  of int   (* CAM の値に対するタグにはCAM_ をつける *)
  | CAM_BoolVal of bool
  | CAM_StringVal of string
  | CAM_ListVal of cam_value list
  | CAM_Fail of string (* エラーの伝播 *)
  | CAM_ClosVal of cam_code * cam_env  (* 再帰関数に対応するクロージャ *)
and cam_stack = cam_value list (* スタック *)
and cam_env = cam_value list (* 環境は、1つのスタックフレームに相当する。 *)

and cam_instr =
  | CAM_Ldi of int                    (* CAM_Ldi(n) は、整数 n をスタックに積む (loadする) *)
  | CAM_Ldb of bool                   (* CAM_Ldb(b) は、真理値 b をスタックに積む (loadする) *)
  | CAM_Lds of string                 (* CAM_Lds(s)は、文字列sをスタックに積む *)
  | CAM_Ldl of cam_code list          (* CAM_Ldl(l)は、リストをスタックに積む *)
  | CAM_Access of int                 (* CAM_Access(i) は、環境の i+1 番目の値をスタックに積む *)
  | CAM_Closure of cam_code           (* CAM_Closure(c) は、関数本体のコードが c で、
                                       * その環境が、現在の環境であるような関数
                                       * クロージャを生成し、それをスタックに積む。
                                       * 前項で説明したように変数は名前の代わりに
                                       * 環境のインデックスで参照されるので、
                                       * このクロージャにも関数引数は含まれない。
                                       * なお、この関数クロージャは、再帰関数で
                                       * あるとして処理される。
                                      *)
  | CAM_Apply                         (* スタックトップの値が関数クロージャならば、
                                       * その関数を、スタックの上から2番めにある値に
                                       * 関数適用した計算を行なう。
                                      *)
  | CAM_Return                        (* 関数の呼び出し元に戻る *)
  | CAM_Let                           (* スタックトップの値を環境の先頭に移す (環境を拡張する) *)
  | CAM_EndLet                        (* 環境の先頭の値を取り除く *)
  | CAM_Test of cam_code * cam_code   (* CAM_Test(c1,c2)は、スタックトップの値が
                                       * true ならば、コードc1 を実行し、false
                                       * ならばコード c2 を実行する。
                                      *)
  | CAM_Eq                            (* スタックトップの値とスタックの上から2番めの値を
                                       * 取り出し、それらが同じ整数であるかどうかテストし、
                                       * その結果の真理値をスタックに積む
                                      *)
  | CAM_NotEq                         (* 同じ値でないことの検査 *)
  | CAM_Not                           (* 真偽値の反転 *)
  | CAM_Add                           (* スタックトップの値とスタックの上から2番めの値を
                                       * 取り出し、その和をスタックに積む
                                      *)
  | CAM_Minus                         (* 差 *)
  | CAM_Times                         (* 積 *)
  | CAM_Div                           (* 商 *)
  | CAM_Greater                       (* 比較 *)
  | CAM_Less                          (* 比較 *)
  | CAM_Cons                          (*
                                        * スタックトップの値と、スタックの上から
                                        * 2番目のリストをつなげてスタックに積む
                                      *)
  | CAM_Head                          (* List.hd *)
  | CAM_Tail                          (* List.tl *)
  | CAM_ListLength                    (* List.length *)
  | CAM_Raise                         (* raise *)
  | CAM_FailWith of string            (* エラーメッセージ付きエラー *)
  | CAM_Try of cam_code                   (*
                                        * スタックトップの値がCAM_Raise若しくは
                                        * CAM_FailWithのときに
                                        * 保持する値を実行する
                                      *)
and cam_code = cam_instr list  (* コードは、命令の列である *)

let rec print_cam_value cam_value =
  match cam_value with
  | CAM_IntVal(n) -> Printf.sprintf "CAM_IntVal(%d)" n
  | CAM_BoolVal(true) -> Printf.sprintf "CAM_BoolVal(true)"
  | CAM_BoolVal(false) -> Printf.sprintf "CAM_BoolVal(false)"
  | CAM_StringVal(s) -> Printf.sprintf "CAM_StringVal(%s)" s
  | CAM_ListVal(l) -> (
    let str =
      l
      |> List.map (fun v -> print_cam_value v)
      |> List.fold_left (fun s1 s2 -> s1 ^ s2 ^ ";") ""
    in
    Printf.sprintf "CAM_ListVal([%s])" str 
  )
  | CAM_Fail(msg) -> Printf.sprintf "CAM_Fail(%s)" msg
  | CAM_ClosVal(_) -> Printf.sprintf "CAM_ClosVal"


let rec print_cam_instr cam_instr =
  match cam_instr with
  | CAM_Ldi(n) -> Printf.sprintf "Ldi %d" n
  | CAM_Ldb(true) -> "Ldb true"
  | CAM_Ldb(false) -> "Ldb false"
  | CAM_Lds(s) -> Printf.sprintf "Lds \"%s\"" s
  | CAM_Ldl(l) -> (
    let s =
      l
      |> List.map (fun code -> (
        code
        |> List.map print_cam_instr
        |> List.fold_left (fun s1 s2 -> s1 ^ s2 ^ ";") ""
      ))
      |> List.fold_left (fun s1 s2 -> s1 ^ "(" ^ s2 ^ "); ") ""
    in
    Printf.sprintf "Ldl [%s]" s
  )
  | CAM_Access(n) -> Printf.sprintf "Access %d" n
  | CAM_Closure(code) -> (
    let s =
      code
      |> List.map print_cam_instr
      |> List.fold_left (fun s1 s2 -> s1 ^ s2 ^ ";") ""
    in
    Printf.sprintf "Closure (%s)" s
  )
  | CAM_Let -> "Let"
  | CAM_EndLet -> "EndLet"
  | CAM_Test(code1, code2) -> (
    let s1 =
      code1
      |> List.map print_cam_instr
      |> List.fold_left (fun s1 s2 -> s1 ^ s2 ^ ";") ""
    in
    let s2 =
      code2
      |> List.map print_cam_instr
      |> List.fold_left (fun s1 s2 -> s1 ^ s2 ^ ";") ""
    in
    Printf.sprintf "Test (%s, %s)" s1 s2
  )
  | CAM_Add -> "Add"
  | CAM_Minus -> "Minus"
  | CAM_Times -> "Times"
  | CAM_Div -> "Div"
  | CAM_Eq -> "Eq"
  | CAM_Less -> "Less"
  | CAM_Greater -> "Greater"
  | CAM_NotEq -> "NotEq"
  | CAM_Not -> "Not"
  | CAM_Cons -> "Cons"
  | CAM_Head -> "Head"
  | CAM_Tail -> "Tail"
  | CAM_ListLength -> "ListLength"
  | CAM_Raise -> "Raise"
  | CAM_FailWith(s) -> Printf.sprintf "FailWith %s" s
  | CAM_Try(code) -> (
    let s =
      code
      |> List.map print_cam_instr
      |> List.fold_left (fun s1 s2 -> s1 ^ s2 ^ ";") ""
    in
    Printf.sprintf "Try (%s)" s
  )
  | CAM_Apply -> "Apply"
  | CAM_Return -> "Return"
