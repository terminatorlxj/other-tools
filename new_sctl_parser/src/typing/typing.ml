type tmodule = {
    mutable name: string;
    mutable filename: string;
    mutable imported: string list;
    mutable definitions: tdefinition list;
    mutable specs: (string * tfml_fair) list;
    mutable type_env: (string, type_expr) Hashtbl.t;
}
and tdefinition = 
    | Tdef_type of string * type_decl
    | Tdef_value of pat * expr
    | Tdef_function of string * function_decl
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
and function_decl = {
    function_params: pat list;
    function_body: expr;
    function_loc: Location.t;
}

let make_function_decl pfunc_decl = 
    {
        function_params = 
            List.map (fun ppat -> make_pat ppat) pfunc_decl.pfunction_params;
        function_body = make_expr pfunc_decl.pfunction_body;
        function_loc = pfunc_decl.pfunction_loc;
    }

let make_type_decl ptype_decl = 
    {
        params = ptype_decl.params;
        arity = ptype_decl.arity;
        kind = begin match ptype_decl.kind with
            | PTKalias pt -> TKalias (make_type pt)
            | PTKvariant sptl -> 
                TKvariant (List.map (fun (s,pt) -> s, make_type pt) sptl)
            | PTKvariant sptl -> 
                TKrecord (List.map (fun (s,pt) -> s, make_type pt) sptl)
            end;
        type_decl_loc = ptype_decl.ptype_decl_loc;
    }

let make_tmodule pmodule = 
    let tdefs = List.map (fun pdef ->
        match pdef with
        | Pdef_type (s, ptype_decl) -> 
            Tdef_type (s, make_type_decl ptype_decl)
        | Pdef_value (ppat, opt, pexpr) ->
            Tdef_value (make_pat ppat, Options.fmap (fun pt -> make_type pt) opt, make_expr pexpr)
        | Pdef_function (s, opt, pfunc_decl) ->
            Tdef_function (s, Options.fmap (fun pt -> make_type pt) opt, make_function_decl pfunc_decl)
    ) pmodule.pdefinitions in
    let tspecs = List.map (fun (s, pfmlf) -> s, make_tfml_fair pfmlf) pmodule.pspecs in
    {
        name = pmodule.name;
        filename = pmodule.filename;
        imported = pmodule.pimported;
        definitions = tdefs;
        specs = tspecs;
        type_env = Hashtbl.create 1;
    }

(*type checking on the top level of the input files*)    
let type_check pmodules dep = 