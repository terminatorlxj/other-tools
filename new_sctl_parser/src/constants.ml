(* type t = 
    | Cint of int
    | Cbool of bool
    | Cfloat of float
    | Cstring of string

let make_const_int i = Cint i
let make_const_bool b = Cbool b
let make_const_float f = Cfloat f
let make_const_string str = Cstring str 

let str_constant const = 
    match const with
    | Cint i -> string_of_int i
    | Cbool b -> string_of_bool b
    | Cfloat f -> string_of_float f
    | Cstring str -> str *)