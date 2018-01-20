open Ptype

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
(*
type ptype = 
    {
        ptype_descr: ptype_descr;
        ptype_loc: Location.t;
    }
and ptype_descr = 
    (* | PTname of Ppath.t *)
    | PTapply of Path.t * (t list)
    | PTarrow of ptype * t
    | PTtuple of ptype list
    | PTlist of ptype
    | PTarray of ptype
    | PTint
    | PTbool
    | PTstring
    | PTfloat
    | PTunit
*)

let rec make_type pt = 
    match pt.ptype_descr with
    | PTapply (path, pt) -> Tapply (path, make_type pt)
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