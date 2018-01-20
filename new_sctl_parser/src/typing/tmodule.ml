type tmodule = {
    mutable name: string;
    mutable filename: string;
    mutable imported: string list;
    mutable definitions: tdefinition list;
    mutable specs: (string * tfml_fair) list;
}
and tdefinition = 
    | Tdef_type of string *  

and type_decl = {
        params: string list;
        arity: int;
        kind: type_kind;
        type_decl_loc: Location.t;
    }
and type_kind = 
    | TKalias of type_expr
    | TKvariant of (string * type_expr) list
    | TKrecord of (string * type_expr) list