
type type_expr = 
    | Tvar of int
    | Tint
    | Tbool
    | Tunit
    | Tstring
    | Tlist of type_expr
    | Tarray of type_expr
    | Tarrow of type_expr * type_expr
    | Ttuple of type_expr list
    | Tapply of Path.t * type_expr
and type_decl = {
        params: string list;
        arity: int;
        kind: type_kind;
    }
and type_kind = 
    | TKalias of type_expr
    | TKvariant of (string * type_expr) list
    | TKrecord of (string * type_expr) list

let count = ref 0 
let new_type_var () = incr count; !count