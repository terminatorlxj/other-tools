
type type_expr = 
    | Tvar of int
    | Tint
    | Tbool
    | Tunit
    | Tstring
    | Tarrow of type_expr * type_expr
    | Ttuple of type_expr list
    | Tapply of Path.t * type_expr
and type_decl = {

    }