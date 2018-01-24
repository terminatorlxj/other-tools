open Pterm
open Pformula

(* Syntactic definitions in a file *)
type pmodule =
    {
        mutable name: string;
        mutable filename: string;
        mutable pimported: string list;
        mutable pdefinitions: pdefinition list;
        mutable pspecs: (string * pfml_fair) list;
    }
and pdefinition = 
    | Pdef_type of string * Ptype.ptype_decl
    | Pdef_value of ppat * (Ptype.ptype option) * pexpr
    | Pdef_function of string * (Ptype.ptype option) * pfunction_decl
and ptype_decl = {
        params: string list;
        arity: int;
        kind: ptype_kind;
        ptype_decl_loc: Location.t;
    }
and ptype_kind = 
    | PTKalias of ptype
    | PTKvariant of (string * ptype) list
    | PTKrecord of (string * ptype) list
and pfunction_decl = 
    {
        pfunction_params: ppat list;
        pfunction_body: pexpr;
        pfunction_loc: Location.t
    }
