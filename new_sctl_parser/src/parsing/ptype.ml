
type ptype = 
    {
        ptype_descr: ptype_descr;
        ptype_loc: Location.t;
    }
and ptype_descr = 
    (* | PTname of Ppath.t *)
    | PTapply of Path.t * (ptype list)
    | PTarrow of ptype * ptype
    | PTtuple of ptype list
    | PTlist of ptype
    | PTarray of ptype
    | PTint
    | PTbool
    | PTstring
    | PTfloat
    | PTunit
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

(* let rec str_type t = 
    match t.type_descr with
    (* | PTvar i -> "Type "^(string_of_int i) *)
    | PTconstr (p, ts) -> (Path.str_path p)^(List.fold_left (fun str t -> str^" "^(str_type t)) "" ts)
    | PTarrow (t1, t2) -> (str_type t1)^"->"^(str_type t2)
    | PTtuple ts -> 
        assert (List.length ts <> 0);
        "("^(str_type (List.hd ts))^(List.fold_left (fun str t -> str^", "^(str_type t)) "" (List.tl ts))^")"
    | PTunit -> "()" *)