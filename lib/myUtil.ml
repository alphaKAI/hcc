open Core_kernel

module StringSet = Set.Make(String)

module CharSet = Set.Make(Char)

let ref_op op r =
  op !r

let ref_op2 op r v =
  op !r v

let effect_op_ref op r =
  r := ref_op op r

let effect_op2_ref op r v =
  r := ref_op2 op r v

let ss_add_ref ss value =
  ss := StringSet.add !ss value

let cs_add_ref cs value = 
  cs := CharSet.add !cs value

let list_add_ref lst value =
  lst := List.append !lst [value]

let list_append_ref lst lst2 =
  lst := List.append !lst lst2

let add_ref_int i v =
  i := !i + v

let (>>) f g x = g(f x)

let rec range i j =
  if i < j then
    i :: range (i + 1) j
  else
    []

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false