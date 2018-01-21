
(* exception Empty_ppath *)

type t = 
    | Pident of string
    | Pdot of string * t

let rec str_path p = 
    match p with
    | Pident str -> str
    | Pdot (str, p1) -> str^"."^(str_path p1)

let rec make_path strs = 
    match strs with
    | [] -> raise Empty_path
    | [str] -> Pident str
    | str::strs1 -> Pdot (str, make_path strs1)

