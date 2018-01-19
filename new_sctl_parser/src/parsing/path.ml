
exception Empty_ppath

type t = 
    | Pident of string
    | Pdot of string * t

let rec str_ppath p = 
    match p with
    | Pident str -> str
    | Pdot (str, p1) -> str^"."^(str_ppath p1)

let rec make strs = 
    match strs with
    | [] -> raise Empty_ppath
    | [str] -> Pident str
    | str::strs1 -> Pdot (str, make strs1)

