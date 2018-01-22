
type ptype = 
    {
        ptype_descr: ptype_descr;
        ptype_loc: Location.t;
    }
and ptype_descr = 
    (* | PTname of Ppath.t *)
    | PTapply of Path.t * (ptype list)
    (* | PTvariant of Path.t * ptype *)
    | PTarrow of ptype * ptype
    | PTtuple of ptype list
    | PTlist of ptype
    | PTarray of ptype
    | PTint
    | PTbool
    | PTstring
    | PTfloat
    | PTunit
and ptype_schema = {
    ptype_schema_descr: ptype_schema_descr;
    ptype_schema_loc: Location.t;
}
(* and ptype_schema_descr = 
    | PTSvar of string
    | PTSapply of Path.t * (ptype_schema list)
    | PTSarrow of ptype_schema * ptype_schema
    | PTStuple of ptype_schema list
    | PTSlist of ptype_schema
    | PTSarray of ptype_schema
    | PTSint 
    | PTSbool
    | PTSstring
    | PTSfloat
    | PTSunit *)
