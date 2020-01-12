open Core
open Hcc
open Compiler
open Serialize
open MyUtil

let numeric_to_lower v =
  let t_size = 64
  and r_size = 8 in
  let ret_len = t_size / r_size in
  range 0 ret_len
  |> List.map ~f:(fun i ->
      Int64.(land) (Int64.(lsr) v (r_size * i)) (Char.max_value |> (Char.to_int >> Int64.of_int))
      |> (Int64.to_int_exn >> Char.of_int_exn))

let run_with_file file_name =
  let compile_result = compile_from_file file_name in
  Printf.printf "[";
  List.iteri ~f:(fun i d ->
      if i > 0 then
        Printf.printf "; ";
      Printf.printf "%s" (show_opCode d)) compile_result;
  Printf.printf "]\n";
  let serialized = serialize_opCodes compile_result in
  let dst_name = file_name ^ ".compiled" in
  Printf.printf "result: [%s]\n" (List.map ~f:show_opCode compile_result |> String.concat ~sep:"; ");
  Printf.printf "bytecodes: [%s]\n" @@ (List.map ~f:Int64.to_string serialized |> String.concat ~sep:", ");
  let fp = Out_channel.create dst_name ~binary:true in
  List.map ~f:numeric_to_lower serialized |> List.concat
  |> List.iter ~f:(fun e ->
      Out_channel.output_char fp e);
  Out_channel.flush fp;
  Out_channel.close fp;
  Printf.printf "compile success [%s -> %s]\n" file_name dst_name

let command =
  Command.basic
    ~summary:"minimal lisp compiler for hvm"
    Command.Let_syntax.(
      let%map_open 
        filename = anon (maybe ("filename" %: string)) in
      match filename with
      | Some fn -> fun () -> run_with_file fn
      | None -> fun () -> ())

let _ = Command.run command 