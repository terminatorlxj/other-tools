
module Skey = struct
    type t = string
    let compare = Pervasives.compare
end;;
module Sset = Set.Make(Skey)

let rec dep_of_pexpr pe =
    match pe.pexpr_descr with
    | 
and dep_of_ppat ppat = 
    match ppat.ppat_descr with
    |
and dep_of_ptype pt = 
    match pt.ptype_descr with
    | 

type dep = (string, Sset.t) Hashtbl.t

let make_dep pmodules = 
    let dep_tbl = Hashtbl.create 1 in
    List.iter (fun pmodule -> 
        let pmname = pmodule.name in
        (*add modules explicitly declared depended*)
        if not (Hashtbl.mem dep pmname) then
            Hashtbl.add dep pmname Sset.empty;
        let deps = ref (Hashtbl.find dep_tbl pmname) in
        List.iter (fun impt -> deps := Sset.add impt !deps) pmodule.pimported;
        Hashtbl.replace dep_tbl pmname !deps
        (*add modules implicitly declared depended in definitions*)
        let add_deps depl = 
            let deps = ref (Hashtbl.find dep_tbl pmname) in
            List.iter (fun dep -> deps := Sset.add dep !deps) depl;
            Hashtbl.replace dep_tbl pmname !deps in
        List.iter (fun pdef ->
            match pdef with
            | Pdef_type (_, ptype_decl) -> begin
                match ptype_decl.kind with
                | PTKalias pt -> add_deps (dep_of_ptype pt)
                | PTKvariant sptl | PTKrecord sptl ->
                    List.iter (fun (_,pt) ->
                        add_deps (dep_of_ptype pt)
                    ) sptl
                end
            | Pdef_value (ppat, None, pexpr) ->
                add_deps ((dep_of_ppat ppat) @ (dep_of_pexpr pexpr))
            | Pdef_value (ppat, Some pt, pexpr) ->
                add_deps ((dep_of_ppat ppat) @ (dep_of_ptype pt) @ (dep_of_pexpr pexpr))
            | Pdef_function (_, None, pfunc_decl) ->
                List.iter (fun ppat -> add_deps (dep_of_ppat ppat)) pfunc_decl.pfunction_params;
                add_deps (dep_of_pexpr pfunc_decl.pfunction_body)
            | Pdef_function (_, Some pt, pfunc_decl) ->
                add_deps (dep_of_ptype pt);
                List.iter (fun ppat -> add_deps (dep_of_ppat ppat)) pfunc_decl.pfunction_params;
                add_deps (dep_of_pexpr pfunc_decl.pfunction_body)
        ) pmodule.pdefinitions;
        (*add modules implicitly declared depended in specs*)

    ) pmodules;
    dep_tbl