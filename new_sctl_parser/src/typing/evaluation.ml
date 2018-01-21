open Term

type runtime = (string, modul) Hashtbl.t
and modul = {
    imported: string list;
    values: (string, value) Hashtbl.t;
    functions: (string, (pat list) * expr) Hashtbl.t;
    mutable specs: (string, fml_fair) list;
}

let make_modul tmodul runtime =
    let imported = tmodul.imported in
    let values = Hashtbl.create 1 
    and functions = Hashtbl.create 1 in
    (*make value bindings*)
    List.iter (fun def -> 
        match def with
        | Tdef_type _ -> ()
        | Tdef_value (pat, expr) -> 
            let value = evaluate expr [] runtime tmodul in
            List.iter (fun (s,val) -> Hashtbl.add values s val) (match_value pat value)
        | Tdef_function (fname, fdecl) -> 
            Hashtbl.add functions fname (fdecl.function_params, fdecl.function_body)
    ) tmodul.definitions;
    let specs = List.map () tmodul.specs in
    {
        imported = imported;
        values = values;
        functions = functions;
        specs = specs;
    }
let make_runtime tmodules dep = 

type ctx = (string * value) list

let evaluate expr ctx runtime current_modul =
    match expr with
    | Expr_int i -> Vint i;
    | Expr_bool b -> Vbool b;
    | 