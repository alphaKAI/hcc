open Core_kernel
open MyUtil

type sexpObject =
  | Float of float
  | Bool of bool
  | String of string
  | Symbol of string
  | List of sexpObject list
  | Object of sexpObject
  | Quote of sexpObject
[@@deriving show, eq, ord]

let sexpObject_kind = (function
    | Float _ -> 0
    | Bool _ -> 1
    | String _ -> 2
    | Symbol _ -> 3
    | List _ -> 4
    | Object _ -> 5
    | Quote _ -> 6) >> Int64.of_int

type parseResult = {
  parse_result: sexpObject option;
  read_len: int
}
[@@deriving show]

let symbol_chars =
  let tmp_char_set = ref CharSet.empty in
  String.iter "~!@#$%^&*-_=+:/?<>" ~f:(fun c -> 
      cs_add_ref tmp_char_set c);
  !tmp_char_set

let next_bracket code left_offset =
  let index = 0
  and left_count = left_offset
  and right_count = 0 in

  let rec loop index left_count right_count =
    if index < String.length code && left_count <> right_count then
      if String.get code index = '(' then
        loop (index + 1) (left_count + 1) right_count
      else if String.get code index = ')' then
        loop (index + 1) left_count (right_count + 1)
      else
        loop (index + 1) left_count right_count
    else
      index in
  loop index left_count right_count

let rec parse_list str =
  let i = ref 1 in
  let lst = ref [] in
  let next_bracket_idx = next_bracket (String.slice str 1 0) 1 in
  let content = String.sub str ~pos:1 ~len:next_bracket_idx in

  let rec loop j = 
    if j < String.length content - 1 then
      let s = String.slice content j 0 in
      let tmp_result = sexp_parseExpr s in
      begin match tmp_result.parse_result with
        | Some p -> list_add_ref lst p
        | None -> ()
      end;
      loop @@ j + tmp_result.read_len
    else
      j in
  let j = loop 0 in 
  add_ref_int i j;
  assert (String.get str !i = ')');
  add_ref_int i 1;
  {
    parse_result = Some(List !lst);
    read_len = !i
  }

and skip_line str =
  let str_len = String.length str in
  let rec loop i =
    if i < str_len && str.[i] <> '\n' then
      loop @@ i + 1
    else
      i in
  let read_len = loop 0 in
  {
    parse_result = None;
    read_len = read_len
  }

and parse_number str = 
  let dot_next_is_number str str_len i =
    (str.[i] = '.' && i + 1 < str_len && is_digit(str.[i + 1])) in
  let i = ref 0 in
  let str_len = String.length str in
  let first =
    if String.get str 0 = '-' then begin
      add_ref_int i 1;
      1
    end else
      0 in

  let rec loop i =
    if i < str_len && (is_digit(str.[i]) || dot_next_is_number str str_len i) then
      loop @@ i + 1
    else
      i in

  let i = loop !i in
  let tmp_len = i - first in
  let tmp = String.sub ~pos:first ~len:tmp_len str in
  let value = float_of_string tmp *. if first = 1 then -1. else 1. in
  {
    parse_result = Some(Float value);
    read_len = i
  }

and parse_symbol str =
  let str_len = String.length str in

  let rec loop i =
    if i < str_len && (is_alpha str.[i] || CharSet.exists symbol_chars ~f:((=) str.[i])) then
      loop @@ i + 1
    else
      i in

  let i = loop 0 in
  let tmp = String.slice str 0 i in
  {
    parse_result = Some(Symbol tmp);
    read_len = i
  }

and parse_string str =
  let str_len = String.length str in

  let rec loop i =
    if i < str_len && str.[i] <> '\"' then
      loop @@ i + 1
    else
      i in

  let i = loop 1 in
  let tmp = String.slice str 1 i in
  {
    parse_result = Some(String tmp);
    read_len = i + 1
  }

and parse_quote str =
  let expr = String.slice str 1 0 |> sexp_parseExpr in
  {
    parse_result = Option.bind ~f:(fun e -> Some(Quote e)) expr.parse_result;
    read_len = 1 + expr.read_len
  }
and sexp_parseExpr code =
  let code_len = String.length code in

  let rec loop i =
    if i < code_len then
      let c = code.[i] in
      match c with
      | ' ' | '\n'
      | '\r' | '\t' -> loop @@ i + 1
      | ';' ->
        let j =
          let t = skip_line @@ String.slice code i 0 in
          t.read_len in
        loop @@ i + j
      | c when is_digit c || (c = '-' && i + 1 < code_len && is_digit code.[i + 1]) ->
        let result = parse_number @@ String.slice code i 0 in
        {
          parse_result = result.parse_result;
          read_len = i + result.read_len
        }
      | c when is_alpha c || CharSet.exists ~f:((=) c) symbol_chars ->
        let result = parse_symbol @@ String.slice code i 0 in
        {
          parse_result = result.parse_result;
          read_len = i + result.read_len
        }
      | c when c = '\"' ->
        let result = parse_string @@ String.slice code i 0 in
        {
          parse_result = result.parse_result;
          read_len = i + result.read_len
        }
      | c when c = '(' ->
        let result = parse_list @@ String.slice code i 0 in
        {
          parse_result = result.parse_result;
          read_len = i + result.read_len
        }
      | c when c = '\'' ->
        let result = parse_quote @@ String.slice code i 0 in
        {
          parse_result = result.parse_result;
          read_len = i + result.read_len
        }
      | _ -> failwith "invalid"
    else
      {
        parse_result = None;
        read_len = i
      }
  in

  loop 0

let sexp_parse code =
  let code_len = String.length code in

  let rec loop i =
    if i < code_len then
      let s = String.slice code i 0 in
      let result = sexp_parseExpr s in
      match result.parse_result with
      | Some v -> v :: (loop @@ i + result.read_len)
      | None -> []
    else
      []
  in

  loop 0