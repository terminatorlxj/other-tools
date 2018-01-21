
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
