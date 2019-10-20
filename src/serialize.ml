open Core_kernel
open Compiler
open MyUtil
open Ksexp

let serialize_string str =
  (String.length str |> Int64.of_int) :: (String.to_list str |> List.map ~f:(int_of_char >> Int64.of_int))

exception Unsupported

let emit_op op = [opCode_kind op]

let debug_show_arr arr =
  List.map ~f:Int64.to_string arr |> String.concat ~sep:", "

let rec serialize_vmValue vmv =
  vmValue_kind vmv ::
  match vmv with
  | VValue v ->
    begin
      sexpObject_kind v ::
      match v with
      | Float num -> [Int64.bits_of_float num]
      | String str -> serialize_string str
      | Symbol str -> serialize_string str
      | Bool b -> [Int64.of_int @@ if b then 1 else 0]
      | List _
      | Object _
      | Quote _ -> raise Unsupported
      (* (List.map ~f:serialize_vmValue >> List.concat) lst *)
    end
  | VFunc vmf ->
    let func_name = serialize_string vmf.func_name in
    let code = serialize_opCodes vmf.func_body in

    let ret = func_name @ (List.length code |> Int64.of_int) :: code @ [List.length vmf.arg_names |> Int64.of_int] in
    ret @ List.bind ~f:serialize_string vmf.arg_names

and serialize_opCodes ops =
  (List.map ~f:serialize_opCode >> List.concat) ops
and serialize_opCode op =
  opCode_kind op ::
  match op with
  | OpPop -> []
  | OpPush v -> serialize_vmValue v
  | OpAllocLvars i
  | OpGetLocal i
  | OpSetLocal i
  | OpSetArgLocal i ->
    [Int64.of_int i]
  | OpFreeVars
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
  | OpPrintln -> []
  | OpJumpRel i -> [Int64.of_int i]
  | OpFuncDef v -> serialize_vmValue (VFunc v)
  | OpCall (func_name, argc) ->
    serialize_string func_name @ [Int64.of_int argc]
  | OpReturn -> []
  | OpVarDef var_name
  | OpGetVar var_name
  | OpSetVar var_name ->
    serialize_string var_name
  | OpBranch i
  | OpMakeList i -> [Int64.of_int i]
  | OpSetArgFrom (arg_name, arg_idx) ->
    serialize_string arg_name @ [Int64.of_int arg_idx]
  | OpDumpEnv -> []