open Ptype

type type_expr = 
    | Tvar of int
    | Tint
    | Tbool
    | Tunit
    | Tfloat
    | Tstring
    | Tlist of type_expr
    | Tarray of type_expr
    | Tarrow of type_expr * type_expr
    | Ttuple of type_expr list
    (* | Tvariant of Path.t * type_expr *)
    | Tapply of Path.t * (type_expr list)



let rec replace_type_vars type_ctx te = 
    match te with
    | Tvar _ | Tint | Tbool | Tunit | Tfloat | Tstring -> te
    | Tapply (Piden s, tel) -> 
        begin try 
            let te1 = Pairs.get_value type_ctx s in
            if tel = [] then te1 else Tapply (Piden s, List.map (fun te -> replace_type_vars type_ctx te) tel)
        with Pairs.Key_not_found -> Tapply (Piden s, List.map (fun te -> replace_type_vars type_ctx te) tel)
        end
    | Tapply (path, tel) -> Tapply (path, List.map (fun te -> replace_type_vars type_ctx te) tel)
    | Tlist te1 -> Tlist (replace_type_vars type_ctx te1)
    | Tarray te1 -> Tarray (replace_type_vars type_ctx te1)
    | Tarrow (te1, te2) -> Tarrow (replace_type_vars type_ctx te1, replace_type_vars type_ctx te2)
    | Ttuple tel -> Ttuple (List.map (fun te -> replace_type_vars type_ctx te) tel)

let rec make_type pt = 
    match pt.ptype_descr with
    | PTapply (path, pt) -> Tapply (path, make_type pt)
    (* | PTapply (path, pt) -> Tapply (path, make_type pt) *)
    | PTarrow (pt1, pt2) -> Tarrow (make_type pt1, make_type pt2)
    | PTtuple ptl -> Ttuple (List.map (fun pt -> make_type pt) ptl)
    | PTlist pt -> Tlist (make_type pt)
    | PTarray pt -> Tarray (make_type pt)
    | PTint -> Tint
    | PTstring -> Tstring
    | PTbool -> Tbool
    | PTfloat -> Tfloat
    | PTunit -> Tunit


let count = ref 0 
let new_type_var () = incr count; !count