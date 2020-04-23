open Core_kernel
open Ksexp
open MyUtil

type vmValue = 
  | VValue of sexpObject
  | VFunc of vmFunction
[@@deriving show, eq, ord]
and vmFunction = {
  func_name: string;
  func_body: opCode list;
  arg_names: string list;
}
[@@deriving show, eq, ord]
and opCode =
  | OpPop
  | OpPush of vmValue
  | OpAllocLvars of int
  | OpFreeVars
  | OpGetLocal of int
  | OpSetLocal of int
  | OpSetArgLocal of int
  | OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpMod
  | OpEq
  | OpNeq
  | OpLt
  | OpLeq
  | OpGt
  | OpGeq
  | OpPrint
  | OpPrintln
  | OpJumpRel of int
  | OpFuncDef of vmFunction
  | OpCall of string * int
  | OpReturn
  | OpVarDef of string 
  | OpGetVar of string
  | OpSetVar of string
  | OpBranch of int
  | OpMakeList of int
  | OpSetArgFrom of string * int
  | OpDumpEnv
[@@deriving show, eq, ord]

let vmValue_kind = (function 
    | VValue _ -> 0
    | VFunc _ -> 1 ) >> Int64.of_int

let opCode_kind = (function
    | OpPop -> 0
    | OpPush _ -> 1
    | OpAllocLvars _ -> 2
    | OpFreeVars -> 3
    | OpGetLocal _ -> 4
    | OpSetLocal _ -> 5
    | OpSetArgLocal _ -> 6
    | OpAdd -> 7
    | OpSub -> 8
    | OpMul -> 9
    | OpDiv -> 10
    | OpMod -> 11
    | OpEq -> 12
    | OpNeq -> 13
    | OpLt -> 14
    | OpLeq -> 15
    | OpGt -> 16
    | OpGeq -> 17
    | OpPrint -> 18
    | OpPrintln -> 19
    | OpJumpRel _ -> 20
    | OpFuncDef _ -> 21
    | OpCall _ -> 22
    | OpReturn -> 23
    | OpVarDef _ -> 24
    | OpGetVar _ -> 25
    | OpSetVar _ -> 26
    | OpBranch _ -> 27
    | OpMakeList _ -> 28
    | OpSetArgFrom _ -> 29
    | OpDumpEnv -> 30) >> Int64.of_int

let opCode_width = function
  | OpPop -> 1
  | OpPush _ -> 2
  | OpAllocLvars _ -> 2
  | OpFreeVars -> 1
  | OpGetLocal _ -> 2
  | OpSetLocal _ -> 2
  | OpSetArgLocal _ -> 2
  | OpAdd -> 1
  | OpSub -> 1
  | OpMul -> 1
  | OpDiv -> 1
  | OpMod -> 1
  | OpEq -> 1
  | OpNeq -> 1
  | OpLt -> 1
  | OpLeq -> 1
  | OpGt -> 1
  | OpGeq -> 1
  | OpPrint -> 1
  | OpPrintln -> 1
  | OpJumpRel _ -> 2
  | OpFuncDef _ -> 2
  | OpCall _ -> 3
  | OpReturn -> 1
  | OpVarDef _ -> 2
  | OpGetVar _ -> 2
  | OpSetVar _ -> 2
  | OpBranch _ -> 2
  | OpMakeList _ -> 2
  | OpSetArgFrom _ -> 3
  | OpDumpEnv -> 1

let opCodes_len = List.map ~f:opCode_width >> List.fold ~init:0 ~f:(fun k v -> k + v)

let is_number t =
  let t1 = int_of_string_opt t
  and t2 = float_of_string_opt t in
  match (t1, t2) with
  | (None, None) -> false
  | _ -> true

let is_string t =
  if String.length t < 2 then
    false
  else
    let c1 = String.get t 0
    and c2 = String.get t (String.length t - 1) in
    let open Char in
    c1 = '\"' && c1 = c2

let fail_invalid ?(msg=None) = 
  failwith @@
  match msg with
  | None -> "invalid source code given"
  | Some t -> Printf.sprintf "invalid source code given - %s" t

let compile_debug = false

let rec compile sexp =
  if compile_debug then
    Printf.printf "sexp: %s\n" @@ show_sexpObject sexp;
  match sexp with
  | Float _ 
  | Bool _  
  | String _
    -> [OpPush (VValue sexp)]
  | Symbol sym
    -> [OpGetVar sym]
  | List (Symbol x :: xs) when String.equal x "def-var" ->
    if List.length xs = 2 then
      let e1 = List.nth_exn xs 0 |> function
        | Symbol v -> v
        | _ -> fail_invalid ~msg:(Some "def-var<1>")
      and e2 = List.nth_exn xs 1 in
      let e2_compiled = compile e2 in
      List.append e2_compiled [OpVarDef e1]
    else
      fail_invalid ~msg:(Some "def-var<2>")
  | List (Symbol x :: xs) when String.equal x "def-fun" ->
    if List.length xs = 3 then
      let func_name = List.nth_exn xs 0 |> function
        | Symbol v -> v
        | _ -> fail_invalid ~msg:(Some "def-fun<1>")
      and e2 = List.nth_exn xs 1 |> function
        | List l -> l
        | _ -> fail_invalid ~msg:(Some "def-fun<2>") in
      let arg_names = List.map ~f:(
          function
          | Symbol t -> t
          | _ -> fail_invalid ~msg:(Some "def-fun<3>")) e2 in
      let func_body =
        List.slice xs 2 0
        |> (List.map ~f:compile >> List.concat) 
      in
      let lvars = ref [] in
      let func_body' =
        let is_OpSetVar = function
          | OpSetVar _ -> true
          | _ -> false
        and is_OpVarDef = function
          | OpVarDef _ -> true
          | _ -> false in
        List.map ~f:(fun op ->
            match op with
            | OpVarDef var_name
            | OpGetVar var_name
            | OpSetVar var_name ->
              let arg_names = List.rev arg_names in
              let found = List.findi ~f:(fun _ e -> String.equal e var_name) arg_names in
              let arg_names_len = List.length arg_names in
              begin match found with
                | None ->
                  if is_OpVarDef op || is_OpSetVar op then
                    let lvar_idx_1 =
                      if is_OpVarDef op then begin
                        list_add_ref lvars var_name;
                        Some (List.length !lvars - 1)
                      end else begin
                        List.findi ~f:(fun _ lvar_name -> String.equal var_name lvar_name) !lvars |> function
                        | None -> None
                        | Some (i, _) -> Some i
                      end in
                    match lvar_idx_1 with
                    | None -> op
                    | Some lvar_idx ->
                      let lvar_idx' = arg_names_len + lvar_idx in 
                      OpSetLocal lvar_idx'
                  else
                    let found_lvar_idx =
                      List.findi ~f:(fun _ lvar_name -> String.equal var_name lvar_name) !lvars |> function
                      | None -> None
                      | Some (i, _) -> Some i in
                    begin match found_lvar_idx with
                      | None -> op
                      | Some lvar_idx ->
                        let lvar_idx' = arg_names_len + lvar_idx in
                        OpGetLocal lvar_idx'
                    end
                | Some (found_idx, _) ->
                  if is_OpVarDef op then 
                    OpSetLocal found_idx
                  else 
                    OpGetLocal found_idx
              end
            | _ -> op) func_body in
      let alloca_size = List.length arg_names + List.length !lvars in
      let func_body = ref [] in
      list_add_ref    func_body @@ OpAllocLvars alloca_size;
      list_append_ref func_body @@ List.mapi ~f:(fun i _ -> OpSetArgLocal i) arg_names;
      list_append_ref func_body @@ func_body';
      if alloca_size > 0 then
        list_add_ref func_body OpFreeVars;
      list_add_ref func_body OpReturn;
      let vmf = {
        func_name = func_name;
        func_body = !func_body;
        arg_names = arg_names
      } in
      [OpFuncDef vmf]
    else
      fail_invalid ~msg:(Some "def-fun<4>")
  | List (Symbol x :: xs) when String.equal x "if" ->
    if List.length xs >= 2 then
      let cond_ins = List.nth_exn xs 0 |> compile in
      let tBlock_ins = List.nth_exn xs 1 |> compile in
      let tBlock_len = opCodes_len tBlock_ins in
      (* offset *)
      let offset = tBlock_len +
                   if List.length xs = 3 (* if fBlock exists, jump offset include amount of jump ins *) then
                     (opCode_width @@ OpJumpRel(0))
                   else 0 in
      let ret = List.append cond_ins (OpBranch offset :: tBlock_ins) in
      if List.length xs = 3 then (* exist fBlock *)
        let fBlock_ins = List.nth_exn xs 2 |> compile in
        let fBlock_len = opCodes_len fBlock_ins in
        List.append ret @@ OpJumpRel (fBlock_len) :: fBlock_ins
      else
        ret
    else
      fail_invalid ~msg:(Some "if")
  | List (Symbol x :: xs) when String.equal x "begin" ->
    if List.length xs >= 1 then
      (List.map ~f:compile >> List.concat) xs
    else
      fail_invalid ~msg:(Some "begin")
  | List (Symbol x :: xs) when String.equal x "while" ->
    if List.length xs = 2 then
      let cond_ins = List.nth_exn xs 0 |> compile
      and expr_ins = List.nth_exn xs 1 |> compile in
      (* offset *)
      let jmp_offset =
        -(
          opCodes_len expr_ins + 2 (* this jump *) +
          opCodes_len cond_ins + 2 (* branch *)
        ) in
      let expr_ins = expr_ins @ [OpJumpRel jmp_offset] in

      let cond_with_branch = List.append cond_ins @@ [OpBranch (opCodes_len expr_ins)] in
      List.append cond_with_branch expr_ins
    else
      fail_invalid ~msg:(Some "while")
  | List (Symbol x :: xs) when String.equal x "for" ->
    (* (for init cond update expr) -> init [cond_with_branch: cond, block_ins: [expr, update]] *)
    if List.length xs = 4 then
      let init_ins = List.nth_exn xs 0 |> compile
      and cond_ins = List.nth_exn xs 1 |> compile
      and update_ins = List.nth_exn xs 2 |> compile
      and expr_ins = List.nth_exn xs 3 |> compile in
      (* offset *)
      let jmp_offset =
        -(
          opCodes_len cond_ins +
          2 (* branch *) +
          opCodes_len expr_ins +
          opCodes_len update_ins +
          2 (* jump to top *)
        ) in
      let block_ins =  expr_ins @ update_ins @ [OpJumpRel jmp_offset] in
      let cond_with_branch = List.append cond_ins [OpBranch (opCodes_len block_ins)] in
      init_ins @ cond_with_branch @ block_ins
    else
      fail_invalid ~msg:(Some "for")
  | List (Symbol x :: xs) when String.equal x "set" ->
    if List.length xs = 2 then
      let var_name = List.nth_exn xs 0 |> function
        | Symbol t -> t
        | _ -> fail_invalid ~msg:(Some "set")
      and expr_ins = List.nth_exn xs 1 |> compile in
      List.append expr_ins [OpSetVar var_name]
    else
      fail_invalid ~msg:(Some x)
  | List (Symbol x :: xs) ->
    let t1 = List.map ~f:compile xs |> List.concat in
    let t2 =
      match x with
      | "+" -> [OpAdd]
      | "-" -> [OpSub]
      | "*" -> [OpMul]
      | "/" -> [OpDiv]
      | "%" -> [OpMod]
      | "==" -> [OpEq]
      | "!=" -> [OpNeq]
      | "<" -> [OpLt]
      | "<=" -> [OpLeq]
      | ">" -> [OpGt]
      | ">=" -> [OpGeq]
      | _ -> [OpCall(x, List.length xs)] in
    List.append t1 t2
  | _ ->
    Printf.printf "given: %s\n" @@ show_sexpObject sexp;
    failwith "invalid source code given ????"

let compile_from_file file_name =
  let content = In_channel.read_all file_name in
  let parsed = sexp_parse content in
  List.map ~f:compile parsed
  |> List.concat
