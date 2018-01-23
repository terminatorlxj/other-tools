
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
    | TKvariant of (Path.t * type_expr) list
    | TKrecord of (string * type_expr) list
and function_decl = {
    function_params: pat list;
    function_body: expr;
    function_loc: Location.t;
}

exception Not_defined of string * string
exception Invalid_path of Path.t

let find_type_def tmodule str = 
    let rec find_in_definitions defs =
    match defs with
    | [] -> None
    | (Tdef_type (s, type_decl))::defs' -> if str = s then Some type_decl else find_in_definitions def'
    | _ :: def' -> find_in_definitions def' in
    find_in_definitions tmodule.definitions

let find_expr_def tmodule str = 
    let rec find_in_definitions defs =
    match defs with
    | [] -> None
    | (Tdef_value (pat, expr))::defs' -> if iden_in_pat s pat then Some expr else find_in_definitions def'
    | _ :: def' -> find_in_definitions def' in
    find_in_definitions tmodule.definitions
    

let rec expand_type_path te tmodule tmodules = 
    match te with
    | Tapply (Piden s, tel) -> 
        begin match find_type_def tmodule s with
        | Some _ -> Tapply (Pdot (tmodule.name, Piden s), List.map (fun te -> expand_type_path te tmodule) tel)
        | None -> 
            let found_in_imported = ref false 
            and founded_tmname = ref "" in
            List.iter (fun tmname ->
                let tmodule = Hashtbl.find tmodules tmname in
                match find_type_def tmodule s with
                | Some _ -> found_in_imported := true; if !founded_tmname = "" then founded_tmname := tmname
                | None -> ()
            ) tmodule.imported;
            if !founded_in_imported then
                Tapply (Pdot (!founded_tmname, Piden s), List.map (fun te -> expand_type_path te tmodule) tel)
            else 
                raise (Not_defined (s, tmodule.name))
        end
    | Tapply (Pdot (tmname, Piden s), tel) -> 
        begin match find_type_def (Hashtbl.find tmodules tmname) s with
        | Some _ -> Tapply (Pdot (tmname, Piden s), List.map (fun te -> expand_type_path te tmodule) tel)
        | None -> raise (Not_defined (s, tmname))
        end
    | Tapply (path, _) -> raise (Invalid_path path)
    | Ttuple tel -> Ttuple (List.map (fun te -> expand_type_path te tmodule) tel)
    | Tarrow (te1, te2) -> Tarrow (expand_type_path te1 tmodule, expand_type_path te2 tmodule)
    | Tarray te1 -> Tarray (expand_type_path te1 tmodule)
    | Tlist te1 -> Tlist (expand_type_path te1 tmodule)
    | Tvar _ | Tint | Tbool | Tunit | Tfloat | Tstring -> te

let rec expand_expr_path e tmodule tmodules = 
    match e.expr_descr with
    | Expr_path (Piden s) -> 
        begin match find_expr_def tmodule s with
        | Some _ -> Expr_path (Pdot (tmodule.name, Piden s))
        | None ->
            let found_in_imported = ref false 
            and founded_tmname = ref "" in
            List.iter (fun tmname ->
                let tmodule = Hashtbl.find tmodules tmname in
                match find_expr_def tmodule s with
                | Some _ -> found_in_imported := true; if !founded_tmname = "" then founded_tmname := tmname
                | None -> ()
            ) tmodule.imported;
            if !found_in_imported then
                Expr_path (Pdot (!founded_tmname, Piden s))
            else
                raise (Not_defined (s, tmodule.name))
        end

let make_type_decl_instance type_decl type_args = 
    let type_ctx = List.zip type_decl.params type_args = 
    match type_decl.kind with
    | TKalias te -> TKalias (replace_type_vars type_ctx te), type_decl.type_decl_loc
    | TKvariant stel -> TKvariant (List.map (fun (s, te) -> s, replace_type_vars type_ctx te) stel), type_decl.type_decl_loc
    | TKrecord stel -> TKrecord (List.map (fun (s, te) -> s, replace_type_vars type_ctx te) stel), type_decl.type_decl_loc

let make_function_decl pfunc_decl = 
    {
        function_params = List.map (fun ppat -> make_pat ppat) pfunc_decl.pfunction_params;
        function_body = make_expr pfunc_decl.pfunction_body;
        function_loc = pfunc_decl.pfunction_loc;
    }

let make_type_decl ptype_decl = 
    {
        params = ptype_decl.params;
        arity = ptype_decl.arity;
        kind = begin match ptype_decl.kind with
            | PTKalias pt -> TKalias (make_type pt)
            | PTKvariant sptl -> TKvariant (List.map (fun (s,pt) -> s, make_type pt) sptl)
            | PTKvariant sptl -> TKrecord (List.map (fun (s,pt) -> s, make_type pt) sptl)
            end;
        type_decl_loc = ptype_decl.ptype_decl_loc;
    }

let make_tmodule pmodule = 
    let tdefs = List.map (fun pdef ->
        match pdef with
        | Pdef_type (s, ptype_decl) -> Tdef_type (s, make_type_decl ptype_decl)
        | Pdef_value (ppat, opt, pexpr) -> Tdef_value (make_pat ppat, Options.fmap (fun pt -> make_type pt) opt, make_expr pexpr)
        | Pdef_function (s, opt, pfunc_decl) -> Tdef_function (s, Options.fmap (fun pt -> make_type pt) opt, make_function_decl pfunc_decl)
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
let check_tmodules tmodules dep = 
    check_