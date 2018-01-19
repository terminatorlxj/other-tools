open Pterm

type pfml_fair = pfml * (pfml list)

and pfml = 
    {
        pfml_descr: pfml_descr;
        pfml_loc: Location.t;
    }
and pfml_descr = 
    | Ptop
    | Pbottom
    | Patomic of string * (pstate list)
    | Pneg of pfml
    | Pand of pfml * pfml
    | Por of pfml * pfml
    | PAX of string * pfml * pstate
    | PEX of string * pfml * pstate
    | PEG of string * pfml * pstate
    | PAF of string * pfml * pstate
    | PEU of string * string * pfml * pfml * pstate
    | PAR of string * string * pfml * pfml * pstate
and pstate =
    {
        pstate_descr: pstate_descr;
        pstate_loc: Location.t;        
    }
and pstate_descr = 
    | Psvar of string
    | Pstate of pexpr
