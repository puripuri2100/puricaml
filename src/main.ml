open Syntax
open Eval
open Typecheck

type arg_value =
  | ArgRun
  | ArgCam
  | ArgZam
  | ArgOutput of string
  | ArgInput of string

let rec parse_argv lst =
  match lst with
  | "-r" :: tail -> ArgRun :: parse_argv tail
  | "--run" :: tail -> ArgRun :: parse_argv tail
  | "-c" :: tail -> ArgCam :: parse_argv tail
  | "--cam" :: tail -> ArgCam :: parse_argv tail
  | "-z" :: tail -> ArgZam :: parse_argv tail
  | "--zam" :: tail -> ArgZam :: parse_argv tail
  | "-o" :: s :: tail -> ArgOutput(s) :: parse_argv tail
  | "--output" :: s :: tail -> ArgOutput(s) :: parse_argv tail
  | "-i" :: s :: tail -> ArgInput(s) :: parse_argv tail
  | "--input" :: s :: tail -> ArgInput(s) :: parse_argv tail
  | s :: tail -> ArgInput(s) :: parse_argv tail
  | [] -> []

let rec find_input_file ans lst =
  match lst with
  | ArgInput(s) :: tail -> (
    if Option.is_some ans then
      failwith "missing arguments"
    else
      find_input_file (Some(s)) tail
  )
  | _ :: tail -> find_input_file ans tail
  | [] -> (
    match ans with
    | Some(ans) -> ans
    | None -> failwith "missing arguments"
  )

let rec find_output_file ans lst =
  match lst with
  | ArgOutput(s) :: tail -> (
    if Option.is_some ans then
      failwith "missing arguments"
    else
      find_output_file (Some(s)) tail
  )
  | _ :: tail -> find_output_file ans tail
  | [] -> (
    match ans with
    | Some(ans) -> Some(ans)
    | None -> None
  )

let rec find_is_run lst =
  match lst with
  | ArgRun::_ -> true
  | _::tail -> find_is_run tail
  | [] -> false

let rec find_is_cam lst =
  match lst with
  | ArgCam::_ -> true
  | _ :: tail -> find_is_cam tail
  | [] -> false

let rec find_is_zam lst =
  match lst with
  | ArgZam::_-> true
  | _ :: tail -> find_is_zam tail
  | [] -> false

let () =
  let argv = Array.to_list Sys.argv in
  let argv =
    match argv with
    | _::l -> l
    | _ -> failwith "missing arguments"
  in
  let argv = parse_argv argv in
  let input_file = find_input_file None argv in
  let is_run = find_is_run argv in
  if is_run then
    (* 実行がCAMかZAMかを探して実行する *)
    let is_cam = find_is_cam argv in
    let is_zam = find_is_zam argv in
    match (is_cam, is_zam) with
    | (true, true) -> failwith "missing arguments"
    | (true, false) -> (
      let input_file_ch = open_in input_file in
      let cam_code =
        Machine_parser.cam Machine_lexer.token (Lexing.from_channel input_file_ch)
      in
      let cam_value = Cam.run_cam_code cam_code in
      let s = Machine.print_cam_value cam_value in
      let () = Printf.printf "%s\n" s in
      ()
    )
    | (false, true) -> (
      let input_file_ch = open_in input_file in
      let zam_code =
        Machine_parser.zam Machine_lexer.token (Lexing.from_channel input_file_ch)
      in
      let zam_value = Zam.run_zam_code zam_code in
      let s = Machine.print_zam_value zam_value in
      let () = Printf.printf "%s\n" s in
      ()
    )
    | (false, false) -> failwith "missing arguments"
  else
    (* 型検査まで行う *)
    let input_file_ch = open_in input_file in
    let exp = Parser.main Lexer.token (Lexing.from_channel input_file_ch) in
    let _ = tinf2top exp in
    let () = close_in input_file_ch in
    (* 出力ファイルの有無 *)
    let output_file = find_output_file None argv in
    match output_file with
    | None -> (
      (* 実行がCAMかZAMかを探して実行する *)
      (* インタプリタ *)
      let env = emptyenv () in
      let initial_continuation = fun a -> a in
      let value = eval exp env initial_continuation initial_continuation in
      let s = print_value 0 value in
      let () = Printf.printf "%s\n" s in
      ()
    )
    | Some(output_file) -> (
      let is_cam = find_is_cam argv in
      let is_zam = find_is_zam argv in
      let output_data =
        match (is_cam, is_zam) with
        | (true, true) -> failwith "missing arguments"
        | (true, false) -> (
          let env = emptyenv () in
          let cam_code = Cam.c_cam exp env in
          let cam_code_str =
            cam_code
            |> List.map Machine.print_cam_instr
            |> List.fold_left (fun s1 s2 -> s1 ^ s2 ^ "; ") ""
          in
          cam_code_str
        )
        | (false, true) -> (
          let env = emptyenv () in
          let zam_code = Zam.c_zam exp env in
          let zam_code_str =
            zam_code
            |> List.map Machine.print_zam_instr
            |> List.fold_left (fun s1 s2 -> s1 ^ s2 ^ "; ") ""
          in
          zam_code_str
        )
        | (false, false) -> failwith "missing arguments"
      in
      let output_file_ch = open_out output_file in
      let () = output_string output_file_ch output_data in
      let () = flush output_file_ch in
      ()
    )
